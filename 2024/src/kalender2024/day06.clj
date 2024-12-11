(ns kalender2024.day06
  (:require [rolfrander.puzzle-lib :as puzzle]))

;                   segment | siffer hvor segment er slukket
(def segment-data ["A"        "14"
                   "B"        "56"
                   "C"        "2"
                   "D"        "147"
                   "E"        "134579"
                   "F"        "1237"
                   "G"        "017"])

(def segments (->> (partition 2 segment-data)
                   (map #(vector (first %)
                                 (reduce (fn [r i] (assoc r (puzzle/char->digit i) false))
                                         (vec (repeat 10 true))
                                         (second %))))
                   (into {})
                   ))

(defn compatible [num-1 seg-1 num-2 seg-2]
  (= (get-in segments [seg-1 num-1])
     (get-in segments [seg-2 num-2])))

(defn check-values [A B D E G I]
  (and (compatible A "C" I "B")
       (compatible B "D" E "G")
       (compatible G "F" D "E")
       (compatible B "A" B "B")))

(time (->> (for [A (range 10)
                 B (range 10)
                 D (range 10)
                 E (range 10)
                 G (range 10)
                 I (range 10)
                 :when (check-values A B D E G I)]
             [A B D E G I])
           (count)
           (* 1000)
           ))
;;=> 134976000


(into {} [["A" 1] ["B" 1]])