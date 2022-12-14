(ns kalender2022.day15 
  (:require [clojure.string :as string]))

(defn parse [input]
  (->> input
       (string/split-lines)
       (rest)
       (map #(string/split % #","))
       (map (fn [p] (mapv #(Long/parseLong %) p)))))

(def testdata
  (parse "value,volume
600,30
1000,40
100,60
1300,70
1400,30
200,100"))

(def data
  (parse (slurp "https://julekalender-backend.knowit.no/challenges/2022-15/files/data.csv")))

(def max-volume 120)
(def max-value 1700)

(defn eval-package [[unpacked available-value available-volume] gift]
  (let [[val vol] gift]
    (if (and (<= val available-value)
             (<= vol available-volume))
      [unpacked (- available-value val) (- available-volume vol)]
      [(conj unpacked gift) available-value available-volume])))

(defn fill-backpack [unpacked]
  (first (reduce eval-package
                 [[] max-value max-volume]
                 unpacked)))


(loop [pakker data
       i 0]
  (if (empty? pakker)
    i
    (recur (fill-backpack pakker) (inc i))))


