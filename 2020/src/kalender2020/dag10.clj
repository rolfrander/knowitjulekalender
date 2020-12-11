(ns kalender2020.dag10
  (:require [clojure.string :as str]))

(set! *unchecked-math* false)
(set! *warn-on-reflection* true)

(def testdata "ae,af,aa,ab,ad,ac
aa,ac,ab,ad,af,ae
ad,ae,ab,aa,af
ad,ac,aa,ab
af,ae,ab,aa,ac")

(def data (slurp "https://julekalender-backend.knowit.no/challenges/10/attachments/leker.txt"))

(defn parse-line [line] 
  (reverse (re-seq #"[a-z]{2}" line)))

(defn score-event [event] 
  (zipmap event (range)))

(defn accumulate-results [maps] 
  (apply merge-with + maps))

(defn find-winner [results] 
  (apply max-key val results))

(time 
 (let [lines (str/split-lines data)]
   (->> (map parse-line lines)
        (map score-event)
        accumulate-results
        find-winner
        (apply format "%s-%d")
        )))
;; => "ti-7776"



(reduce-kv (fn [t key value]
             (update t key #(if % (+ % value) value))) {"a" 3} {"a" 1, "b" 2})

((partial + 3) 2)

(apply format "%s-%d" ["ae" 11])

(merge-with + {"a" 3} {"a" 1, "b" 2})

(let [lines (str/split-lines testdata)]
  (->> (map parse-line lines)
       (map (fn [event]
              ))))

(reverse (re-seq #"[a-z]{2}" "ab,cd,ef"))