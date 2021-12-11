(ns kalender2021.day11
  (:require [clojure.string :as string]))

(set! *warn-on-reflection* true)
(set! *unchecked-math*  :warn-on-boxed)

(def names (string/split-lines (slurp "https://julekalender-backend.knowit.no/rails/active_storage/blobs/redirect/eyJfcmFpbHMiOnsibWVzc2FnZSI6IkJBaHBBcDhDIiwiZXhwIjpudWxsLCJwdXIiOiJibG9iX2lkIn19--4e38dd2802132b5f356a3018edad90e70c24e93e/names.txt?disposition=inline")))

(def santa-list (string/split-lines (slurp "https://julekalender-backend.knowit.no/rails/active_storage/blobs/redirect/eyJfcmFpbHMiOnsibWVzc2FnZSI6IkJBaHBBcUFDIiwiZXhwIjpudWxsLCJwdXIiOiJibG9iX2lkIn19--8e26125e06ecb2664bba74b49be7bd0727ca8373/locked.txt?disposition=inline")))


(defn name-mangle [input-name]
  (let [c (count input-name)
        namevec (vec input-name)]
    (->> (for [i (range 0 c)]
           (for [j (range 0 c)]
             (cond (= i (dec c)) j
                   (= i j) (inc j)
                   (= (inc i) j) (dec j)
                   :else j)))
         (map #(replace namevec %))
         (map #(apply str %)))))

(defn crypto-match [^String line ^String check-name]
  (letfn [(match-name [^String n]
            (let [name-length (.length n)
                  line-length (.length line)]
              (loop [i 0
                     j 0
                     start 99]
                (cond (= j name-length)   (- i start name-length)
                      (= i line-length)   nil
                      (= (.charAt line i)
                         (.charAt n j))   (recur (inc i) (inc j) (min start i))
                      :else               (recur (inc i) j start)))))]

    (if-let [match-length (some match-name (name-mangle check-name))]
      [check-name match-length]
      nil)))

(defn check-valid [name-extra-list]
  (case (count name-extra-list)
    0 nil
    1 (first (first name-extra-list))
    (let [sorted-list (sort-by second name-extra-list)]
      (if (not= (second (first sorted-list))
                (second (second sorted-list)))
        (first (first sorted-list))
        nil))))

(def decoded-list
  (for [input santa-list
        :let [candidate (->> names
                             (map #(crypto-match input %))
                             (remove nil?)
                             check-valid)]
        :when (not (nil? candidate))]
    [input candidate]))

(->> (map second decoded-list)
     frequencies
     (sort-by second >)
     first)
