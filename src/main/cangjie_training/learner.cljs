(ns cangjie-training.learner
  (:require [cangjie-training.util :refer [days-diff log
                                           rescale time-from-now]]))

;;; Spaced repetition learning method impl

(defprotocol SpacedRepetitionStat
  (grade-answer [this parts-score])
  (update-stat [this grade])
  (compare-stat [this other])
  (due-date [this])
  (need-review? [this])
  (need-hint? [this])
  (seems-learnt? [this])
  (stat-viz-hiccup [this]))

(defrecord SM-2 [n ef i]
  SpacedRepetitionStat
  ;; SM-2 algorithm
  ;; n = repetitions, ef = previous ease factor, i = previous interval
  ;; ref. https://www.supermemo.com/en/archives1990-2015/english/ol/sm2
  (grade-answer [_this parts-score]
    (let [part-count (count parts-score)]
      ;; each part can be scored -1~1
      (-> (apply + parts-score) ; sum of part scores as word score
          ; rescale to 0~5 for SM-2 algorithm
          (rescale [(* -0.5 part-count) part-count] [0 5])
          js/Math.round)))
  (update-stat [_this grade]
    (let [ef (+ ef (- 0.1 (* (- 5 grade) (+ 0.08 (* (- 5 grade) 0.02)))))
          _ (log "update-stat"
                 (- 0.1 (* (- 5 grade) (+ 0.08 (* (- 5 grade) 0.02))))
                 (+ -0.8 (* 0.28 grade) (* 0.02 (* grade grade))))
          ef (max ef 1.3)]
      (if (>= grade 3)
        (SM-2. (inc n) ef (case n, 0 1, 1 6, (js/Math.round (* i ef))))
        (SM-2. 0 ef i))))
  (compare-stat [{i1 :i n1 :n ef1 :ef} {i2 :i n2 :n ef2 :ef}]
    (if (not= i1 i2) (< i1 i2) (if (not= n1 n2) (< n1 n2) (< ef1 ef2))))
  (due-date [_this] ; TODO set due date
    (js/Date.now))
  (need-review? [_this] ; TODO need to review criteria
    true)
  (need-hint? [{:keys [ef]}] ; TODO find good threshold for hinting
    (>= ef 2))
  (seems-learnt? [_this] ; TODO define learnt
    false)
  (stat-viz-hiccup [_this]
    {"n ACS2" n "ef ASC3" (.toFixed ef 2) "i ASC1" i}))
(defn new-SM-2 [] (SM-2. 0 2.5 0))

(defrecord SM-2-mod [dbr difficulty dlr po]
  SpacedRepetitionStat
  ;; modified SM-2
  ;; see https://www.blueraja.com/blog/477/a-better-spaced-repetition-learning-algorithm-sm2
  (grade-answer [_this parts-score]
    (let [part-count (count parts-score)]
      ;; each part can be scored -1~1
      (-> (apply + parts-score) ; sum of part scores as word score
          ; rescale to 0~1 for SM2+ algorithm
          (rescale [(* -0.5 part-count) part-count] [0 1]))))
  (update-stat [_this rating]
    (let [now (js/Date.now)
          correct? (> rating 0.6)
          overdue (if correct?
                    (min 2 (/ (days-diff now dlr) dbr))
                    1)
          difficulty (+ difficulty (* overdue (* (/ 1 17) (- 8 (* 9 rating)))))
          difficulty (max 0 (min difficulty 1)) ; clamp between 0 ~ 1
          difficulty-weight (- 3 (* 1.7 difficulty))
          dbr (if correct?
                (+ 1 (* (dec difficulty-weight) overdue (+ 0.95 (rand 0.1))))
                (min 1 (/ 1 (+ 1 (* 3 difficulty)))))]
      (SM-2-mod. dbr difficulty now overdue)))
  (compare-stat [a b]
    (or (nil? (:dlr a)) (< (due-date a) (due-date b))))
  (due-date [_this]
    (+ dlr (* dbr 1000 60 60 24)))
  (need-review? [this]
    (<= (due-date this) (js/Date.now)))
  (need-hint? [this]
    (not (seems-learnt? this)))
  (seems-learnt? [_this]
    (< difficulty 0.3))
  (stat-viz-hiccup [this]
    {"mastery"
     (let [mastery (- 1 difficulty)
           max-rating 3
           num-icons 3
           rating (rescale mastery [0 1] [0 max-rating])
           half-ratio (quot max-rating num-icons)
           rated-icons (concat (repeat (quot rating half-ratio) "🌕")
                               (repeat (mod rating half-ratio) "🌗"))]
       [:span (if (and (< mastery 0.1) (seems-learnt? this))
                (repeat max-rating "🌚")
                (concat rated-icons
                        (repeat (- num-icons (count rated-icons)) "🌑")))
        #_(str (if (some? mastery) (.toFixed mastery 2) ""))])
     "last review" (if (some? dlr)
                     [:span (time-from-now dlr)]
                     "never")
     "due" [:span (let [due-date (due-date this)]
                    (if (and (some? dlr) (> due-date (js/Date.now)))
                      [:span (time-from-now due-date)]
                      "now"))]
     ;"familiar" [:span.opacity-50 (.toFixed dbr 2)]
     }))

(defn new-SM-2-mod [difficulty]
  (SM-2-mod. 1 difficulty nil 1))
#_(map new-SM-2-mod (take 10 cj-dict/popular-chinese-chars))
