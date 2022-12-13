(ns kalender2022.day09 
  (:require [clojure.string :as string]))

(set! *unchecked-math* false)

(defn liter-julebrus [temperatur vann kullsyre]
  (if-not (and (<= 95 temperatur 105)
               (<= 400 vann 1500)
               (<= 300 kullsyre 500))
    0
    (let [brus (+ (- vann 100)
                  (Math/floor (/ kullsyre 10)))
          damp (if (>= temperatur 100)
                 (Math/floor (/ brus 40))
                 0)]
      (- brus damp))))

; (liter-julebrus 101 500 300)

(def input (slurp "https://julekalender-backend.knowit.no/challenges/2022-09/files/julebrusmaskiner.txt"))

(defn parse-longs [l]
  (let [parse (fn [i]
                (if (re-matches #"\d+" i)
                  (Long/parseLong i)
                  i))]
    (map parse l)))

(defn parse-data [data]
  (->> (string/split-lines data)
       (map #(rest (re-matches #"Maskin ([^,]+), temperatur (\d+)C, vann (\d+)L, kullsyre (\d+)L" %)))
       (map parse-longs)
       (group-by first)))

(def data (parse-data input))

(defn calc-list [list]
  (->> (map (comp (partial apply liter-julebrus) rest) list)
       (reduce +)))

(let [machine->volume (map #(vector (first %) (calc-list (second %))) data)
      max-machine (apply max-key second machine->volume)
      tot-vol (reduce + (map second machine->volume))]
  (println (int tot-vol) (first max-machine)))

(calc-list (second (first data)))