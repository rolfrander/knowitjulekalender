(ns src.kalender2024.day02
  (:require [clojure.string :as str]
            [rolfrander.puzzle-lib :as puzzle]))

(let [max-prime 10000000
      primes ^booleans (puzzle/prime-sieve max-prime)]
  (defn is-prime? [look-for-number]
    (aget primes look-for-number))

  (def primes (filter is-prime? (range 2 max-prime))))

(defn tverrsum [n]
  (->> n
       (format "%d")
       (map puzzle/char->digit)
       (reduce +)))

(time
 (->> primes
      (map-indexed #(vector %2 (tverrsum (inc %1)) (tverrsum %2)))
      (filter (fn [[a b c]] (if (= b c) true nil)))
      (map first)
      (take 10000)
      (reduce +)
      ))
;;=> 18857085298
