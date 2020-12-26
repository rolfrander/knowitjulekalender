(ns kalender2020.day23
  (:require [clojure.string :as str]))

(set! *unchecked-math* false)
(set! *warn-on-reflection* true)

(def ^:dynamic *debug* false)

(def testwords "snø 5
hygge 7
grøt 10
pepperkake 6
nisse 2")

(def testrap "Lil Niz X: nisse pepperkake hygge
Nizzy G: grøt pepperkake grøt grøt
Lil Niz X: nisse julenisse nisse pepperkake
Nizzy G: julegrøt snø")

(defn parse-words [input]
  (->> input
       str/split-lines
       (map #(str/split % #" "))
       (map (fn [[w p]] [w (Integer/parseInt p)]))
       (into {})))

(def vowels (into #{} "aeiouyæøå"))

(defn count-vowels [word]
  (count (filter vowels word)))

(defn calculate-points [word-points rap-words]
  (let [result
        (first
         (reduce (fn [[points vowels-previous previous-word prev-cnt] ^String word]
                   (let [base-word (if (str/starts-with? word "jule")
                                    (.substring word 4)
                                    word)
                         vowel-multiplier (if (str/starts-with? word "jule") 2 1)
                         vowels (count-vowels word)
                         vowel-diff (max 0 (- vowels vowels-previous))
                         repeat (if (= base-word previous-word) (inc prev-cnt) 1)
                         base-points (get word-points base-word 0)
                         word-points (quot (+ base-points (* vowel-multiplier vowel-diff)) repeat)]
                     (when *debug* (println word word-points))
                    
                     [(+ points word-points)
                      vowels
                      base-word
                      repeat]))
                 
                 [0 100 "" 0]
                 rap-words))]
    
    (when *debug* (println rap-words result))
    result))

(defn exec [rap words]
  (let [word-points (parse-words words)]
    (->> rap
         str/split-lines
         (map #(str/split % #": "))
         (map (fn [[name rap-words]]
                [name (calculate-points word-points (str/split rap-words #" "))]))
         ;(group-by first)
         (reduce (fn [results [name points]]
                   (update results name #(if % (+ points %) points)))
                 {})
         (apply max-key val)
         (str/join ",")
         )))

(binding [*debug* true]
  (exec testrap testwords))

(exec 
 (slurp "https://julekalender-backend.knowit.no/challenges/23/attachments/rap_battle.txt")
 (slurp "https://julekalender-backend.knowit.no/challenges/23/attachments/basewords.txt"))
;; => "Nizzy G,3316"

