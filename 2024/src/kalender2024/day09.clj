(ns kalender2024.day09
  (:require [clojure.string :as str]))

(def data (slurp "https://julekalender-backend.knowit.no/challenges/2024-09/files/tall.txt?disposition=inline"))

(defn num-to-string [num]
  (case num
    2  "to"
    4  "fire"
    6  "seks"
    8  "åtte"
    12 "tolv"
    18 "atten"
    20 "tjue"
    30 "tretti"
    40 "førti"
    50 "femti"
    60 "seksti"
    70 "sytti"
    80 "åtti"
    90 "nitti"
    (str/join [(num-to-string (* (quot num 10) 10))
               (num-to-string (mod num 10))])))

(def num-map (->> (range 6 100 6)
                  (map (juxt num-to-string identity))
                  (into {})))

(def re-num #"seks(?!ti)|tolv|atten|tjuefire|tretti|førtito|førtiåtte|femtifire|seksti|syttito|syttiåtte|åttifire|nitti")

(->> (re-seq re-num data)
     (map num-map)
     (reduce +)
     ((fn [x] (/ x 6))))

; ************************

(defn next-num [s start]
  (condp #(.startsWith s %1 %2) start
    "seksti" [60 6]
    "seks" [6 4]
    "tolv" [12 4]
    "atten" [18 5]
    "tjuefire" [24 8]
    "tretti" [30 6]
    "førtito" [42 7]
    "førtiåtte" [48 9]
    "femtifire" [54 9]
    "syttito" [72 7]
    "syttiåtte" [78 9]
    "åttifire" [84 8]
    "nitti" [90 5]))

(let [f (fn [[sum pos]] (when (< pos (count data))
                          (let [[sum-delta pos-delta] (next-num data pos)]
                            [(+ sum sum-delta) (+ pos pos-delta)])))]
  (-> (take-while seq (iterate f [0 0]))
      last
      first
      (/ 6)))

