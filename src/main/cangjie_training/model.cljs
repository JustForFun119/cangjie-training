(ns cangjie-training.model
  (:require [clojure.string :as str]
            [cangjie-training.dictionary :refer [popular-chinese-chars
                                                 radical-dict]]
            [cangjie-training.learner :as learner]
            [cangjie-training.util :refer [hours-diff log rescale]]))

;;; app model

(def init-learn-char-count 20)
(def learn-more-word-count 10)
(def viz-page-size-compact 3)
(def viz-page-size-long 10)

(def qwerty-key->cj-part
  {"q" "手", "w" "田", "e" "水", "r" "口", "t" "廿", "y" "卜", "u" "山", "i" "戈",
   "o" "人", "p" "心", "a" "日", "s" "尸", "d" "木", "f" "火", "g" "土", "h" "竹",
   "j" "十", "k" "大", "l" "中", "x" "難", "c" "金", "v" "女", "b" "月", "n" "弓",
   "m" "一"})

(defn split-radicals [question-char]
  (-> (radical-dict question-char) (str/split "")))

(defn chinese-char-lookup-url [chinese-character]
  #_(str "https://www.chinesecj.com/cjdict/index.php?stype=Word&detail=y&sword="
         chinese-character)
  (str "https://www.hkcards.com/cj/cj-char-" chinese-character ".html"))

(defn persist-learner-db! [db-json]
  (->> db-json
       clj->js
       js/JSON.stringify
       (js/localStorage.setItem "cj-training-db-json")))
#_(persist-learner-db! {"一" {:dbr 1 :d 0.3 :dlr (js/Date.now) :po 1}})

(defn load-learner-db! [map->learner-db-item]
  (let [db (-> (js/localStorage.getItem "cj-training-db-json")
               js/JSON.parse
               (js->clj :keywordize-keys true))]
    (reduce-kv (fn [m k v]
                 ; also convert string key instead of keyword key
                 (-> m (dissoc k) (assoc (name k) (map->learner-db-item v))))
               db db)))
#_(load-learner-db! identity)

(defn backup-db! [db-json]
  (->> db-json
       clj->js
       js/JSON.stringify
       (js/localStorage.setItem "cj-training-db-json-bak")))

(defn init-learner-db [items]
  (->> items
       (map (fn [char]
              ; difficulty based on number of radicals for the character
              (let [difficulty (rescale (count (split-radicals char))
                                        [1 5] [0.1 1])]
                [char #_(new-SM-2)
                 (learner/new-SM-2-mod difficulty)])))
       (into (hash-map))))
#_(cljs.pprint/pprint (init-learner-db
                       (take init-learn-char-count popular-chinese-chars)))

(defonce *learner-db
  (atom
   (if-let [db (load-learner-db! learner/map->SM-2-mod)]
     (do (backup-db! db) db)
     (init-learner-db (take init-learn-char-count
                            popular-chinese-chars)))))

(defn new-chars-to-learn [n learner-db]
  (->> popular-chinese-chars
       (filter #(not (contains? learner-db %)))
       (take n)))
#_(new-chars-to-learn 10)
#_(init-learner-db (new-chars-to-learn 10))
#_(merge @*learner-db (init-learner-db (new-chars-to-learn 10)))

(defn items-to-review [learner-db]
  (->> learner-db (filter (fn [[_char stat]] (learner/need-review? stat)))))

(defn review-in-next-hours [learner-db next-hours]
  (let [now (js/Date.now)]
    (->> learner-db vals
         (map #(hours-diff (learner/due-date %) now))
         (filter #(<= % next-hours)))))

(defn learner-progress [learner-db]
  (let [items (vals learner-db)
        total (count items)
        num-need-hint (->> items (filter learner/need-hint?) count)]
    (/ (- total num-need-hint) total)))
#_(learner-progress @*learner-db)

(defn prompt-learn-more? [learner-db add-count]
  (when (seq (new-chars-to-learn add-count learner-db))
    (let [num-learnt (->> learner-db
                          vals
                          (filter learner/seems-learnt?)
                          count)]
      (>= num-learnt (* (count learner-db) 0.8)))))
#_(can-learn-more? @*learner-db)


;;; Chinese character input Q&A

(defn next-question [model learner-db]
  ;; sort DB to show upcoming card, by how much need to review
  (if-let [review-items (seq (items-to-review learner-db))]
    (let [new-char (->> review-items (sort-by val learner/compare-stat)
                        first key)]
      (assoc model :question-char new-char :ans-parts [] :hint-count 0
             :parts-score (-> new-char split-radicals count (repeat 0) vec)
             :answered? false
             :question-start-time (js/Date.now)
             :viz-page-size viz-page-size-compact))
    (do (log "no item to review!")
        (assoc model :question-char nil :ans-parts [] :hint-count 0
               :parts-score nil :answered? false
               :viz-page-size viz-page-size-long :viz-page 1))))
#_(->> @*learner-db
       (sort-by val compare-stat)
       (filter (fn [[_char stat]] (need-review? stat))))

(defn new-model [learner-db]
  (-> {:viz-page-size (if (seq (items-to-review learner-db))
                        viz-page-size-compact
                        viz-page-size-long)
       :viz-page 1
       :show-stats? false}
      (merge {:question-char nil
              :ans-parts []
              :hint-count 0
              :parts-score []
              :answered? false})
      (next-question learner-db)))

(defn score-part [parts-score part-index correct?]
  (let [score (nth parts-score part-index)]
    (assoc parts-score part-index
           (if (zero? score) ; part score default zero = scorable
             ; +1 for correct part answer, -1 if incorrect
             (if correct? (inc score) (dec score))
             ; don't update score if part has been answered
             score))))

(defn score-part-hint [parts-score part-index]
  (let [score (nth parts-score part-index)]
    (assoc parts-score part-index
           (if (zero? score) ; part score default zero = scorable
             -0.5 ; -0.5 for asking hint
             ; don't update score if part has been answered
             score))))

(defn has-next-page? [{:keys [viz-page-size viz-page]} learner-db]
  (< (* viz-page viz-page-size) (count learner-db)))