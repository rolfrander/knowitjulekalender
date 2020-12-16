(ns kalender2020.dag15
  (:require [clojure.string :as str])
  (:import (java.util ArrayList
                      Collections)))

(def testdata "innovasjons, løsheta
spektral, sikringens
verdens, spillet   ")

(defn read-words []
    (with-open [in (clojure.java.io/reader "dag15-wordlist.txt")]
      (reduce (fn [wordlist word] 
                (.add wordlist word)
                wordlist)
              (ArrayList.)
              (line-seq in))))

(def words (read-words))

(defn find-prefix
  "finds all words in the wordlist having 'word' as prefix"
  [word]
  (let [match (Math/abs (Collections/binarySearch words word))]
    (for [n (range)
          :let [candidate (.get words (+ n match))]
          :while (.startsWith candidate word)]
      candidate)))

(defn find-word
  "returns true if word exists in wordlist"
  [word]
    (>= (Collections/binarySearch words word) 0))

(defn find-bridges [a b]
  (let [prefix-len (count a)]
    (->>
     (find-prefix a)
     (map #(.substring (str %) prefix-len))
     (filter #(find-word %))
     (filter #(find-word (str/join [% b]))))))

(defn parse [input]
  (let [m (re-matcher #"([a-zæøå]+), ([a-zæøå]+)" input)]
    (->> (partial re-find m)
         repeatedly
         (take-while seq)
         (map rest))))

(time 
 (->> (slurp "dag15.txt")
      parse
      (map (partial apply find-bridges))
      (reduce (partial into) #{})
      (map count)
      (reduce +)))

(time 
 (->> (slurp "dag15-wordlist.txt")
      parse
      count
      ))