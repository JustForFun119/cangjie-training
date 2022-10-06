(ns cangjie-training.event-fx
  (:require [clojure.string :as str]
            [cljs.core.async :as async]
            [rum.core :as rum]
            [cangjie-training.dictionary :as cj-dict]
            [cangjie-training.learner :as learner]
            [cangjie-training.model :as model :refer [*learner-db]]
            [cangjie-training.util :refer [log]]))

(defn- key-event->msg [model key-event]
  (let [{:keys [question-char ans-parts hint-count answered? show-stats?]} model
        radicals   (model/split-radicals question-char)
        char-input (.-key key-event)
        key-pressed (.-code key-event)]
    (cond
      ;; question answered: get next chinese character
      (and answered? (= key-pressed "Space"))
      [:msg/question-answered]

      ;; delete last entered char
      (and (not answered?) (= key-pressed "Backspace"))
      [:msg/delete-last-char]

      ;; add entered char
      (and (< (count ans-parts) (count radicals))
           (contains? model/qwerty-key->cj-part char-input))
      [:msg/add-entered-char char-input]

      ;; show hint for next char
      (and (not= ans-parts radicals) (< hint-count (count radicals))
           (= key-pressed "Tab"))
      [:msg/show-hint]

      (and show-stats? (= key-pressed "BracketRight"))
      [:msg/viz-next-page]

      (and show-stats? (= key-pressed "BracketLeft"))
      [:msg/viz-prev-page]

      (= key-pressed "Slash")
      [:msg/set-stats-show-hide (not show-stats?)]

      :else [])))

(defn- update-model
  "update app model provided keyboard input event 
   (update : Msg -> Model -> [Model Effect])"
  [model [message-type & msg-args]]
  (let [{:keys [question-char ans-parts hint-count parts-score viz-page]} model
        radicals (model/split-radicals question-char)]
    (case message-type

      ;; generate new question: ask user to answer next card
      :msg/new-question
      [(let [[learner-db] msg-args] (model/next-question model learner-db))
       nil]

      ;; question answered: update learner DB, then generate new question
      :msg/question-answered
      [model
       {:fx-type :fx/update-learner-db
        :post-fx (fn [learner-db] [[:msg/save-learner-db learner-db]
                                   [:msg/new-question learner-db]])}]

      ;; delete last entered char
      :msg/delete-last-char
      [(assoc model
              :ans-parts  (-> ans-parts butlast vec)
              :hint-count (max (dec hint-count) 0))
       nil]

      ;; add entered char
      :msg/add-entered-char
      (let [[char-input]   msg-args
            ans-part-index (count ans-parts)
            new-ans-parts  (conj ans-parts char-input)
            correct?       (= char-input (nth radicals ans-part-index))]
        [(assoc model
                :ans-parts   new-ans-parts
                :hint-count  (max hint-count (count new-ans-parts))
                :parts-score (model/score-part parts-score ans-part-index
                                               correct?)
                :answered?   (= new-ans-parts radicals))
         nil])

      ;; show hint for next char
      :msg/show-hint
      [(assoc model
              :hint-count  (min (inc hint-count) (count radicals))
              :parts-score (model/score-part-hint parts-score hint-count))
       nil]

      ;; save learner DB somewhere
      :msg/save-learner-db
      [model
       {:fx-type :fx/persist-learner-db
        :learner-db (let [[learner-db] msg-args] learner-db)}]

      ;; pagination: next page
      :msg/viz-next-page
      [(assoc model :viz-page (if (model/has-next-page? model @model/*learner-db)
                                (inc viz-page)
                                viz-page))
       nil]

      ;; pagination: previous page
      :msg/viz-prev-page
      [(assoc model :viz-page (if (> viz-page 1) (dec viz-page) viz-page))
       nil]

      :msg/expand-learner-pool
      [model
       (let [[expand-count] msg-args]
         {:fx-type :fx/expand-learner-pool
          :expand-count expand-count
          :post-fx (fn [learner-db] [[:msg/save-learner-db learner-db]
                                     [:msg/new-question learner-db]])})]

      :msg/set-stats-show-hide
      [(assoc model :show-stats? (first msg-args))
       nil]

      [model nil])))

(defmulti do-effect! (fn [_model {:keys [fx-type]}] fx-type))

(defmethod do-effect! :fx/update-learner-db
  [{:keys [question-char parts-score]} {:keys [post-fx]}]
  (let [grade (learner/grade-answer (get @*learner-db question-char)
                                    parts-score)]
    (post-fx (swap! *learner-db update question-char
                    learner/update-stat grade))))

(defmethod do-effect! :fx/persist-learner-db
  [_db {:keys [learner-db]}]
  (log "saving..." (clj->js learner-db))
  (model/persist-learner-db! learner-db))

(defmethod do-effect! :fx/expand-learner-pool
  [_db {:keys [expand-count post-fx]}]
  (if-let [new-chars (seq (model/new-chars-to-learn expand-count 
                                                    @model/*learner-db))]
    (when (js/confirm
           (str "Add these " expand-count " characters to words pool:\n"
                (str/join " " new-chars)
                "\n"
                "Are you sure?"))
      (log "adding" expand-count "items to learner db pool" (str new-chars))
      (post-fx (swap! *learner-db merge (model/init-learner-db new-chars))))
    (js/alert "You have learnt all " (count cj-dict/popular-chinese-chars)
              " Chinese characters available in this app! 🎉")))

(defn init-event-msg-chan [*model *pressed-keys]
  (let [>message-chan (async/chan)]
    ;; app event loop
    (async/go-loop []
      (swap! *model (fn [model message]
                      (let [[next-model effect] (update-model model message)]
                        (when effect (doseq [msg (do-effect! model effect)]
                                       (async/put! >message-chan msg)))
                        next-model))
             (async/<! >message-chan))
      (recur))
    ;; hook keyboard event
    (let [on-key-up  (fn [event]
                       (async/put! >message-chan (key-event->msg @*model event))
                       (swap! *pressed-keys disj (.-code event))
                       (.preventDefault event)
                       false)
          on-key-down (fn [event]
                        ; disable Tab key moving keyboard focus away from page
                        (when (#{"Tab" "/"} (.-key event)) (.preventDefault event))
                        ;; (log (.-code event))
                        (swap! *pressed-keys conj (.-code event)))]
      (rum/use-effect! ; react hook
       (fn []
         (.addEventListener js/document "keyup" on-key-up)
         (.addEventListener js/document "keydown" on-key-down)
         (fn []
           (.removeEventListener js/document "keyup" on-key-up)
           (.removeEventListener js/document "keydown" on-key-down)))
       []))
    ;; return async channel
    >message-chan))
