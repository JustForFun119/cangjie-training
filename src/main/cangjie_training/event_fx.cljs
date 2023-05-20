(ns cangjie-training.event-fx
  (:require [cangjie-training.dictionary :as cj-dict]
            [cangjie-training.learner :as learner]
            [cangjie-training.model :as model :refer [*learner-db]]
            [cangjie-training.util :refer [log]]
            [cljs.core.async :as async]
            [clojure.set :as set]
            [clojure.string :as str]
            [goog.string :as gstring]
            [goog.string.format]
            [rum.core :as rum]))


(def key-code->name
  {"KeyQ" "q" "KeyW" "w" "KeyE" "e" "KeyR" "r" "KeyT" "t" "KeyY" "y" "KeyU" "u"
   "KeyI" "i" "KeyO" "o" "KeyP" "p" "KeyA" "a" "KeyS" "s" "KeyD" "d" "KeyF" "f"
   "KeyG" "g" "KeyH" "h" "KeyJ" "j" "KeyK" "k" "KeyL" "l" "KeyZ" "z" "KeyX" "x"
   "KeyC" "c" "KeyV" "v" "KeyB" "b" "KeyN" "n" "KeyM" "m"
   "Tab" "Tab" "Backspace" "Backspace" "BracketLeft" "BracketLeft"
   "BracketRight" "BracketRight" "Space" "Space" "Slash" "Slash"})

(defn- key-event->msg [model key-code]
  (cond
    (= key-code "Space") [:msg/checked-answer]
    (= key-code "Backspace") [:msg/delete-last-char]
    (= key-code "Tab") [:msg/show-hint]
    (= key-code "BracketRight") [:msg/viz-next-page]
    (= key-code "BracketLeft") [:msg/viz-prev-page]
    (= key-code "Slash") [:msg/set-stats-show-hide (not (:show-stats? model))]
    :else (let [key-name (key-code->name key-code)]
            (if (contains? model/keyboard-key->cj-part key-name)
              [:msg/enter-char key-name]
              []))))

(defn key-event [model key-name]
  (key-event->msg model (get (set/map-invert key-code->name) key-name)))

(defn- update-model
  "update app model provided keyboard input event 
   (update : Msg -> Model -> [Model Effect])"
  [model [message-type & msg-args]]
  (let [{:keys [question-char ans-parts hint-count parts-score answered?
                show-stats? viz-page]} model
        radicals (model/split-radicals question-char)]
    (case message-type

      ;; generate new question: ask user to answer next card
      :msg/new-question
      [(let [[learner-db] msg-args] (model/next-question model learner-db))
       nil]

      ;; question answered: update learner DB, then generate new question
      :msg/checked-answer
      (if answered?
        [model
         {:fx-type :fx/update-learner-db
          :post-fx (fn [learner-db] [[:msg/save-learner-db learner-db]
                                     [:msg/new-question learner-db]])}]
        [model nil])

      ;; delete last entered char
      :msg/delete-last-char
      (if (not answered?)
        [(assoc model
                :ans-parts  (-> ans-parts butlast vec)
                :hint-count (max (dec hint-count) 0))
         nil]
        [model nil])

      ;; add entered char
      :msg/enter-char
      (if (< (count ans-parts) (count radicals))
        (let [[char-input]   msg-args
              ans-part-index (count ans-parts)
              correct?       (= char-input (nth radicals ans-part-index))
              new-ans-parts  (conj ans-parts char-input)]
          [(assoc model
                  :ans-parts   new-ans-parts
                  :hint-count  (max hint-count (count new-ans-parts))
                  :parts-score (model/score-part parts-score ans-part-index
                                                 correct?)
                  :answered?   (= new-ans-parts radicals))
           nil])
        [model nil])

      ;; show hint for next char
      :msg/show-hint
      (if (and (not= ans-parts radicals)
               (< hint-count (count radicals)))
        [(assoc model
                :hint-count  (min (inc hint-count) (count radicals))
                :parts-score (model/score-part-hint parts-score hint-count))
         nil]
        [model nil])

      ;; save learner DB somewhere
      :msg/save-learner-db
      [model
       {:fx-type :fx/persist-learner-db
        :learner-db (let [[learner-db] msg-args] learner-db)}]

      ;; pagination: next page
      :msg/viz-next-page
      (if show-stats?
        [(assoc model :viz-page (if (model/has-next-page? model @model/*learner-db)
                                  (inc viz-page)
                                  viz-page))
         nil]
        [model nil])

      ;; pagination: previous page
      :msg/viz-prev-page
      (if show-stats?
        [(assoc model :viz-page (if (> viz-page 1) (dec viz-page) viz-page))
         nil]
        [model nil])

      :msg/expand-learner-pool
      [model
       (let [[expand-count prompt-message] msg-args]
         {:fx-type :fx/expand-learner-pool
          :expand-count expand-count
          :prompt-message prompt-message
          :post-fx (fn [learner-db] [[:msg/save-learner-db learner-db]
                                     [:msg/new-question learner-db]])})]

      :msg/set-stats-show-hide
      [(assoc model :show-stats? (first msg-args))
       nil]

      [model nil])))

(defmulti do-effect! (fn [_model {:keys [fx-type]}] fx-type))

(defmethod do-effect! :fx/update-learner-db
  [{:keys [question-char parts-score question-start-time]} {:keys [post-fx]}]
  (let [grade (learner/grade-answer (get @*learner-db question-char)
                                    parts-score)
        answer-time-taken-ms (- (js/Date.now) question-start-time)]
    (post-fx (swap! *learner-db update question-char
                    learner/update-stat grade (/ answer-time-taken-ms 1000)))))

(defmethod do-effect! :fx/persist-learner-db
  [_db {:keys [learner-db]}]
  (log "saving..." (clj->js learner-db))
  (model/persist-learner-db! learner-db))

(defmethod do-effect! :fx/expand-learner-pool
  [_db {:keys [expand-count prompt-message post-fx]}]
  (if-let [new-chars (seq (model/new-chars-to-learn expand-count
                                                    @model/*learner-db))]
    (when (js/confirm
           (gstring/format prompt-message expand-count (str/join " " new-chars)))
      (log "adding" expand-count "items to learner db pool" (str new-chars))
      (post-fx (swap! *learner-db merge (model/init-learner-db new-chars))))
    (js/alert "You have learnt all " (count cj-dict/popular-chinese-chars)
              " Chinese characters available in this app! 🎉")))

(defn init-event-msg-chan [*model *pressed-keys]
  (let [>message-chan (async/chan)]
    ;; app event loop
    (async/go-loop []
      (->> (async/<! >message-chan)
           (swap! *model (fn [model message]
                           (let [[next-model effect] (update-model model message)]
                             (when effect (doseq [msg (do-effect! model effect)]
                                            (async/put! >message-chan msg)))
                             next-model))))
      (recur))
    ;; hook keyboard event
    (let [on-key-up
          (fn [event]
            (async/put! >message-chan (key-event->msg @*model (.-code event)))
            (swap! *pressed-keys disj (.-code event))
            (.preventDefault event)
            false)
          on-key-down
          (fn [event]
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
