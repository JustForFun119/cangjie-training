#!/usr/bin/env bb

(require '[clojure.string :as str])

(println "Converting CangJie dictionary to EDN...")

(let [dict-str (slurp "./src/cangjie5.dict.yaml")
      dict-map (->> (subs dict-str (str/index-of dict-str "..."))
                    (str/split-lines)
                    (drop 2)
                    (map #(vec (take 2 (str/split % #"\t"))))
                    (filter (fn [[_char parts]]
                              ; no 難(X) / 重(Z) characters
                              (and (not (str/starts-with? parts "x"))
                                   (not (str/starts-with? parts "z")))))
                    (into (hash-map)))]
  (spit "./src/cangjie5.dict.edn" dict-map))

(println "Done!")