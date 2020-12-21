(ns kalender2020.day22
  (:require [clojure.string :as str]))

(set! *unchecked-math* false)
(set! *warn-on-reflection* true)

(def testdata "llmnmgimnaaiechhchajghefgjkudri [Michael, Guri, Aksel]")

(defn parse-line [line]
  (let [[_ letters names] (re-matches #"([^ ]+) \[([^\]]+)\]" line)
        names (str/split (str/lower-case names) #", ")]
    [letters names]))

(defn parse [input]
  (->> (str/split-lines input)
       (map parse-line)))

(defn highest-index [v]
  (first (apply max-key second (map-indexed vector v))))

(defn check [[letters names]]
  (loop [l (seq letters)
         [n & names] names
         i 0]
    (if (nil? n)
      i
      (let [[remaining-letters remaining-name] (reduce (fn [[unused-letters name] next-letter]
                                                         (if (= (first name) next-letter)
                                                           [unused-letters (rest name)]
                                                           [(conj unused-letters next-letter) name]))
                                                       [[] (seq n)]
                                                       l)]
        (if (empty? remaining-name)
          (recur remaining-letters names (inc i))
          (recur l names i))))))

(->> (parse (slurp "https://julekalender-backend.knowit.no/challenges/22/attachments/input.txt"))
     (map check)
     highest-index)
