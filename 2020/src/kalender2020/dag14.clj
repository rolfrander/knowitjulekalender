(ns kalender2020.dag14)


(defn ^booleans sieve [cnt]
  (let [result (boolean-array cnt true)]
    (aset-boolean result 0 false)
    (aset-boolean result 1 false)
    (doseq [i (range 2 (Math/sqrt cnt))]
      (when (aget result i)
        (doseq [j (range (* i i) cnt i)]
          (aset-boolean result j false))))
    result))

(let [primes ^booleans (sieve 15330000)]
  (defn is-prime? [look-for-number]
    (aget primes look-for-number)))

(defn plus-minus-game [length]
  (let [[primecount seen prev2 prev1]
        (reduce (fn [[primecount seen prev2 prev1] current]
                  ;(println primecount prev2 prev1 current)
                  (let [next-minus (- prev2 current)
                        next-plus (+ prev2 current)
                        next (if (and (> next-minus 0)
                                      (not (contains? seen next-minus)))
                               next-minus
                               next-plus)]
                    [(if  (is-prime? next)
                       (inc primecount) 
                       primecount)
                     (conj seen next) prev1 next]))
                [0 #{0 1} 0 1]
                (range 2 length))]
    primecount 
    ))

(time 
 (plus-minus-game 1800813))
;; => 67321



