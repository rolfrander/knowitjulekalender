(ns kalender2024.day08
  (:require [rolfrander.puzzle-lib :as puzzle]))

(def is-prime? (puzzle/get-prime-sieve 10000000))

(loop [primes (filter is-prime? (range))
       last-primtalv 0
       cnt 0]
  (let [cur-prime-digits (puzzle/siffer (first primes))
        primtalv (+ last-primtalv (apply + (map * cur-prime-digits (iterate #(* 10 %) 1))))]
    (if (> primtalv 10000000)
      cnt
      (recur (rest primes) primtalv (if (is-prime? primtalv) (inc cnt) cnt)))))
     
