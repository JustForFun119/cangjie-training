(ns cangjie-training.util
  (:require [clojure.string :as str]))

;;; utility

(defn log [& args]
  (apply js/console.log (str "[" (namespace ::x) "]") args))
#_(log "hello")

(defn rescale [x [old-min old-max] [new-min new-max]]
  (-> (- x old-min)
      (/ (- old-max old-min))
      (* (- new-max new-min))
      (+ new-min)))

(defn days-diff [date-1 date-2]
  (/ (- date-1 date-2) (* 1000 60 60 24)))
#_(days-diff (js/Date.parse "2022-06-17 12:00")
             (js/Date.parse "2022-06-12 6:00"))

(defn hours-diff [datetime-1 datetime-2]
  (/ (- datetime-1 datetime-2) (* 1000 60 60)))
#_(hours-diff (js/Date.parse "2022-06-17 12:00")
              (js/Date.parse "2022-06-17 10:00"))

(defn format-datetime [date-time]
  (-> (js/Intl.DateTimeFormat. "en-HK" #js {"dateStyle" "short"
                                            "timeStyle" "short"})
      (.format date-time)))
#_(format-datetime (js/Date.now))

(defn clean-relative-format [format-parts]
  (->> format-parts
       (map js->clj)
       (keep (fn [part] (when (#{"integer" "literal"} (get part "type"))
                          (get part "value"))))
       (str/join "")))
#_(clean-relative-format
   (.formatToParts (js/Intl.RelativeTimeFormat. "en-HK" #js {"numeric" "auto"})
                   (/ 27 24) "day"))

(defn time-from-now [datetime]
  (let [hours (hours-diff datetime (js/Date.now))
        fmt (js/Intl.RelativeTimeFormat. "en-HK" #js {"numeric" "auto"})]
    (if (> (js/Math.abs hours) 24)
      (clean-relative-format (.formatToParts fmt (/ hours 24) "day"))
      (if (> (js/Math.abs hours) 1)
        (clean-relative-format (.formatToParts fmt hours "hour"))
        (if (pos? hours) "soon" "recently")))))
#_(time-from-now (js/Date.parse "2022-06-17 12:00"))
#_(time-from-now (- (js/Date.now) (* 1000 60 60 2)))
#_(time-from-now (- (js/Date.now) (* 1000 60 60 26)))
#_(time-from-now (js/Date.now))
