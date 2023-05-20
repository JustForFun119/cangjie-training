(ns cangjie-training.ui
  (:require [cangjie-training.dictionary :as cj-dict]
            [cangjie-training.event-fx :as event-fx]
            [cangjie-training.learner :as learner]
            [cangjie-training.model :as model]
            [cangjie-training.util :refer [log]]
            [cljs.core.async :as async]
            [clojure.set :as set]
            [clojure.string :as str]
            [rum.core :as rum]))

;;; app UI components

(defonce *pressed-keys (atom #{}))

(def keyboard-key->hud
  {"Tab" "Tab ⭾"
   "Backspace" "Backspace <-"
   "Space" "Space ␣"
   "BracketLeft" "["
   "BracketRight" "]"
   "Slash" "/"})

(rum/defc card < rum/static [content]
  [:div {:class ["bg-white rounded-lg"
                 "p-4 ring-1 ring-slate-900/5 shadow-md"
                 "dark:bg-slate-700"]}
   content])

(rum/defc button < rum/static [label on-click-fn]
  [:button.px-3.py-1
   {:on-click on-click-fn
    :class ["bg-slate-200 border border-gray-400"
            "dark:bg-slate-600 dark:text-slate-100"]}
   label])

(rum/defc progress-bar < rum/static [progress-percent]
  [:div {:class "w-full bg-gray-200 rounded-full h-2"}
   [:div {:class "bg-blue-600 h-2 rounded-full"
          :style {:width (str progress-percent "%")}}]])

(rum/defc keyboard-prompt < rum/reactive
  ([keyboard-key]
   [:div.flex.items-center.justify-center.rounded
    {:class ["h-10 px-3 py-2 md:px-4"
             (when (= 1 (count (keyboard-key->hud keyboard-key)))
               "aspect-square")
             "text-black dark:text-white drop-shadow"
             (if ((rum/react *pressed-keys) keyboard-key)
               "bg-slate-300 dark:bg-gray-600"
               "bg-slate-200 dark:bg-gray-700")]}
    [:span (keyboard-key->hud keyboard-key)]])
  ([keyboard-key label]
   [:div.flex.flex-row.items-center
    {:class ["px-2 py-2 md:px-3 bg-slate-300"
             "dark:bg-slate-500 dark:active:bg-slate-400 dark:border-gray-200"]}
    (keyboard-prompt keyboard-key)
    [:div {:class "pl-2 md:pl-3 font-semibold text-black dark:text-slate-100"}
     label]]))


;;; Cangjie radical Q&A UI
(rum/defc keyboard-key < rum/reactive [eng-char highlight?]
  (let [pressed? ((rum/react *pressed-keys)
                  (str "Key" (str/upper-case eng-char)))]
    [:div.relative.border.border-slate-300
     {:class ["text-xl md:text-2xl dark:border-slate-600"
              (when highlight? "font-semibold bg-green-300 dark:bg-green-600")
              (when pressed? "bg-gray-300 dark:bg-gray-500")]
      :style {:max-width "9vw" :height "5rem" :aspect-ratio "1"}}
     [:div.absolute.top-0.left-0.capitalize
      {:class ["leading-none"
               (when (or (= eng-char "f") (= eng-char "j"))
                 "underline decoration-gray-300 dark:decoration-gray-600")]
       :style {:padding "min(2vw, 10px) 0 0 min(1vw, 10px)"}}
      eng-char]
     [:div.absolute.bottom-0.right-0
      {:class "leading-none"
       :style {:padding "0 min(1vw, 10px) min(2vw, 10px) 0"}}
      (model/qwerty-key->cj-part eng-char)]]))

(rum/defc keyboard-row < rum/static [chars hint-char]
  [:div.w-full.flex.flex-row.justify-center
   (for [eng-char (str/split chars "")]
     (rum/with-key (keyboard-key eng-char (= hint-char eng-char)) eng-char))])

(rum/defc keyboard-hud < rum/static
  [& {:keys [question-char ans-parts hint-count show?]}]
  (let [radicals (model/split-radicals question-char)
        hint-char (when (> hint-count (count ans-parts))
                    (nth radicals (count ans-parts) nil))]
    [:div.w-full.flex.flex-col {:class (when-not show?
                                         "bg-slate-200 dark:bg-slate-800")
                                :style {:height "16rem"}}
     (when show? [:<> ; QWERTY keyboard only
                  (keyboard-row "qwertyuiop" hint-char)
                  (keyboard-row "asdfghjkl" hint-char)
                  (keyboard-row "xcvbnm" hint-char)])]))

(rum/defc char-answer-part < rum/static
  [& {:keys [radical hint? next? answered? correct?]}]
  [:div
   [:span.inline-flex.items-center.justify-center
    {:class ["w-12 md:w-16 p-2 border-b-8 text-5xl md:text-6xl"
             (if next? "border-slate-400" "border-transparent")
             (when answered? (if correct?
                               "border-green-800 dark:border-green-600"
                               "border-red-800 dark:border-red-600"))]}
    (if hint? "?" (model/qwerty-key->cj-part radical))]])

(rum/defc char-question-parts < rum/static [radicals ans-parts hint-count]
  [:div.flex.flex-row.gap-4.items-center
   (for [index (range (count radicals))]
     (let [ans (nth ans-parts index nil)
           part (nth radicals index nil)
           correct? (= ans part)]
       (rum/with-key (char-answer-part
                      :radical   (if (or (nil? ans) correct?) part ans)
                      :hint?     (and (nil? ans) (<= hint-count index))
                      :next?     (= (count ans-parts) index)
                      :answered? (> (count ans-parts) index)
                      :correct?  correct?)
         index)))])

(rum/defc char-question-controls < rum/static [radicals ans-parts hint-count]
  [:div.flex.flex-row.flex-wrap.items-center {:class "gap-3 md:gap-5"}
   ;; display keyboard control: hint for character part
   (when (< hint-count (count radicals))
     (keyboard-prompt "Tab" "hint"))
   ;; display keyboard control: delete last char, if answer has wrong char
   (when (and (pos? (count ans-parts))
              (seq (set/difference (set (take (count ans-parts) radicals))
                                   (set ans-parts))))
     (keyboard-prompt "Backspace" "delete"))
   ;; display keyboard control: play next word
   (when (= ans-parts radicals)
     [:<>
      [:span.text-2xl.px-4 "✔️"]
      (keyboard-prompt "Space" "next word")])])

(rum/defc radical-question < rum/static [question-char ans-parts hint-count]
  (let [radicals (model/split-radicals question-char)]
    [:<>
     [:span {:class "text-6xl md:text-7xl"}
      [:a.hover:text-blue-600.hover:underline
       {:href (model/chinese-char-lookup-url question-char)
        :target "_blank"
        :title (str "Find " question-char " on www.hkcards.com")}
       question-char]]
     (char-question-parts radicals ans-parts hint-count)
     [:div.pt-4 (char-question-controls radicals ans-parts hint-count)]]))

(rum/defc learn-more-prompt < rum/static [add-count >event-chan]
  [:div (button
         (str "Learn " add-count " more words")
         #(async/put! >event-chan [:msg/expand-learner-pool add-count]))])

(rum/defc learner-db-viz < rum/static [{:keys [viz-page-size viz-page] :as model}
                                       learner-db >event-chan]
  (let [learner-db            (sort-by val learner/compare-stat learner-db)
        table-cell-style      "border border-slate-300 dark:border-slate-500"
        learn-db-item-example (-> learner-db vals first)
        header                (concat ["Character"]
                                      (map name (keys (learner/stat-viz-hiccup
                                                       learn-db-item-example))))
        start-index           (* (dec viz-page) viz-page-size)
        items-page            (->> learner-db
                                   (drop start-index)
                                   (take viz-page-size))
        pagination
        [:div.flex.flex-row.items-center.justify-end
         (when (> viz-page 1)
           [:div.font-bold (keyboard-prompt "BracketLeft")])
         [:span.px-2 (str (inc start-index)
                          "~" (+ start-index (count items-page))
                          " / " (count learner-db))]
         (when (model/has-next-page? model @model/*learner-db)
           [:div.font-bold (keyboard-prompt "BracketRight")])]]

    [:div.flex.flex-col.gap-4
     ;; describe: next items to be reviewed
     [:div.flex.flex-row.items-center.justify-between
      ;; describe: learning item pool
      [:div {:style {:flex "3"}}
       (str "Learning " (count @model/*learner-db) " of "
            (count cj-dict/popular-chinese-chars) " ")
       [:a {:class ["underline"
                    "hover:text-blue-600 hover:underline visited:text-purple-600"
                    "dark:text-blue-400"]
            :href "https://humanum.arts.cuhk.edu.hk/Lexis/lexi-can/faq.php"
            :title "Source"
            :target "_blank"}
        "popular Chinese words"]]
      [:div {:style {:flex "1"}} pagination]]
     ;; learner visualisation
     [:table.table-auto.border-collapse.text-center
      {:class [table-cell-style "text-sm md:text-base"]}
      [:thead [:tr (for [text header]
                     [:th {:key text :class [table-cell-style
                                             "font-semibold"]} text])]]
      [:tbody
       (for [[char learn-stat] items-page]
         [:tr {:key char
               :class (when (learner/need-review? learn-stat)
                        "bg-blue-300 dark:bg-blue-800")}
          [:td {:key char :class ["px-4" table-cell-style]} char]
          (for [text (map val (learner/stat-viz-hiccup learn-stat))]
            [:td {:key text :class ["px-4" table-cell-style]} text])])]]
     [:div.flex.flex-row.flex-wrap.items-center.gap-x-4.gap-y-2
      ;; learner progress
      (let [progress-percent (* (model/learner-progress @model/*learner-db) 100)]
        [:div.flex.flex-row.items-center.gap-2 {:class "w-6/12 md:w-4/12"}
         "Progress"
         (progress-bar progress-percent)
         (str (js/Math.floor progress-percent) "%")])
      ;; button prompt to add more chars into learning pool
      (learn-more-prompt model/learn-more-word-count >event-chan)]]))

(rum/defc learner-stats < rum/static [{:keys [show-stats?] :as model} >event-chan]
  [:div.flex.flex-col.gap-4
   [:details {:open show-stats?
              :on-toggle #(async/put! >event-chan
                                      [:msg/set-stats-show-hide
                                       (-> % .-target .-open)])}
    [:summary {:class ["px-2 text-gray-500 font-semibold"
                       "hover:bg-slate-400 hover:text-slate-100"
                       "dark:hover:bg-slate-700"
                       "active:bg-slate-400 dark:active:bg-slate-600"]
               :tab-index -1}
     [:div.py-1.inline-flex.flex-row.items-center.gap-2
      [:span {:class ["dark:text-slate-100"]}
       (if show-stats? "Hide Stats" "Show Stats")]
      (keyboard-prompt "Slash")]]
    [:div {:class "md:px-8"}
     (learner-db-viz model @model/*learner-db >event-chan)]]
      ;; chinese character stat quick peek
   #_(let [{:keys [parts-score answered?]} model
           stat (get @*learner-db question-char)]
       [:div
        [:div (str (js->clj stat))]
        (when answered?
          [:div (str "grade: " (grade-answer stat parts-score))])])])

(rum/defc no-review-yet < rum/static [model >event-chan]
  (let [next-hours 8]
    [:div.w-full.h-full.grid.justify-items-center.items-center
     {:style {:grid-template-rows "1fr 1fr" :gap "1rem"}}
     [:div {:style {:min-width "fit-content" :width "80vw" :max-width "36rem"}}
      (card
       [:div.py-8.flex.flex-col.items-center.gap-8
        [:div.text-2xl
         [:p "Nothing to review yet."]
         [:p "Check again tomorrow!"]]
        [:p.text-xl.italic
         (let [items-next-up (model/review-in-next-hours @model/*learner-db
                                                         next-hours)]
           (str (if (seq items-next-up)
                  (str (count items-next-up)
                       (if (> (count items-next-up) 1) " items" " item"))
                  "None")
                " in next " next-hours " hours"))]
        ; prompt user to learn more words
        (when (model/prompt-learn-more? @model/*learner-db
                                        model/learn-more-word-count)
          (learn-more-prompt model/learn-more-word-count >event-chan))])]
      ;; learner DB visualisation
     [:div.w-full.px-4.self-end {:style {:max-width "52rem"}}
      (learner-stats model >event-chan)]]))

(rum/defc radical-trainer < rum/reactive [*model >event-chan]
  (let [{:keys [question-char ans-parts hint-count] :as model}
        (rum/react *model)
        review-items (model/items-to-review @model/*learner-db)]
    (if (nil? question-char)
      (no-review-yet model >event-chan)
      [:div.w-full.h-full.grid.justify-items-center.items-center
       {:style {:grid-template-rows "5fr 2fr 3fr" :gap "1rem"}}
       [:div {:style {:min-width "fit-content" :width "80vw" :max-width "36rem"}}
        (card [:div.flex.flex-col.items-center
               [:div.flex.flex-col.items-center {:class "p-2" :style {:gap "1rem"}}
                [:span (str (count review-items) " remaining")]
                (radical-question question-char ans-parts hint-count)]])]
       ;; learner DB visualisation
       [:div.w-full.px-4.self-end {:style {:max-width "52rem"}}
        (learner-stats model >event-chan)]
       ;; show virtual keyboard
       (keyboard-hud :question-char question-char
                     :ans-parts ans-parts
                     :hint-count hint-count
                     :show? (or (some-> @model/*learner-db (get question-char)
                                        learner/need-hint?)
                                (> hint-count 0)))])))

;; app main
(rum/defc app-main < rum/static [*model]
  (let [>message-chan (event-fx/init-event-msg-chan *model *pressed-keys)]
    ;; base UI component
    [:div.w-full.h-screen.p-4.flex.flex-col.justify-center
     {:class ["bg-slate-50 dark:bg-slate-900 dark:text-slate-100"]}
     (radical-trainer *model >message-chan)]))