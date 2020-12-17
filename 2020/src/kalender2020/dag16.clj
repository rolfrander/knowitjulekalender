(ns kalender2020.dag15)

(defn factorize-recursive [num]
  (case num
    1 []
    2 [2]
    3 [3]
    4 [2 2]
    5 [5]
    6 [2 3]
    (loop [[cand & rest] (range 2 (unchecked-divide-int num 2))]
      (if (= 0 (mod num cand))
        (conj (factorize-recursive (unchecked-divide-int num cand))
              cand)
        (if (seq rest) (recur rest)
            [num])))))

(defn factorize [num]
  (->> (range 2 (inc (/ num 2)))
       (filter #(= 0 (mod num %)))))

(defn factor-sum [num]
  (+ 1 num (apply + (factorize num))))


(time
 (->> (range 1 1000000)
      (map #(- (factor-sum %) (* 2 %)))
      (filter (partial < 0))
      (filter quadratic)
      count))

;; **** alternativ løsning ****
;; https://stackoverflow.com/questions/2267146/what-is-the-fastest-integer-factorization-algorithm

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn ^booleans prime-sieve [cnt]
  (let [result (boolean-array cnt true)]
    (aset-boolean result 0 false)
    (aset-boolean result 1 false)
    (doseq [i (range 2 (Math/sqrt cnt))]
      (when (aget result i)
        (doseq [j (range (* i i) cnt i)]
          (aset-boolean result j false))))
    result))

(let [primes ^booleans (prime-sieve 1000000)]
  (defn is-prime? [look-for-number]
    (aget primes look-for-number)))

(defn gcd [^long a ^long b]
  (if (= b 0) a
      (recur b (long (mod a b)))))

;;;; sieve
(defn sigma-sieve [iterations]
  (let [sigma (int-array iterations -1)]
    (aset-int sigma 0 0)
    (aset-int sigma 1 1)
    (aset-int sigma 2 3)
    (aset-int sigma 3 4)
    (aset-int sigma 4 7)
    (doseq [i (range 2 iterations)]
      (if (is-prime? i)
        ;; dersom primtall
        (let [sigma-i (inc i)]
          ; sigma(i^n) = sigma(i^(n-1))+i^n when i is prime
          (loop [i-to-n i
                 sigma-i-to-n (inc i)]
            (aset-int ^ints sigma i-to-n sigma-i-to-n)
            (let [next (* i-to-n i)]
              (when (< next iterations) (recur next (+ sigma-i-to-n next)))))

          ; sigma(i*j) = sigma(i) * sigma(j) when gcd(i,j)=1
          (doseq [j (range 2 (min i (inc (quot iterations i))))]
            (let [sigma-j (aget ^ints sigma j)]
              (if (> sigma-j 0)
                (aset-int sigma (* j i) (* sigma-j sigma-i))))))
        ;; dersom ikke primtall
        (let [sigma-i (aget ^ints sigma i)]
          (if (> sigma-i 0)
            (doseq [j (range 2 (min i (Math/ceil (/ iterations i))))]
              (when (= 1 (gcd i j))
                (aset-int sigma (* j i) (* sigma-i
                                           (aget ^ints sigma j)))))))))
    sigma))


;;;; sieve
(defn sigma-sieve [iterations]
  (let [sigma (int-array iterations -1)]
    (aset-int sigma 0 0)
    (aset-int sigma 1 1)
    (doseq [i (range 2 iterations)]
      (if (is-prime? i)
        ;; dersom primtall
        ; sigma(i^n) = sigma(i^(n-1))+i^n when i is prime
        (loop [i-to-n i
               sigma-i-to-n (inc i)]
          (aset-int ^ints sigma i-to-n sigma-i-to-n)
          (let [next (* i-to-n i)]
            (when (< next iterations) (recur next (+ sigma-i-to-n next))))))

      (let [sigma-i (aget ^ints sigma i)]
        ; sigma(i*j) = sigma(i) * sigma(j) when gcd(i,j)=1
        (doseq [j (range 2 (min i (Math/ceil (/ iterations i))))]
          (when (= 1 (gcd i j))
            (aset-int sigma (* j i) (* sigma-i
                                       (aget ^ints sigma j)))))))
    sigma))

(defn quadratic [num]
  (let [sqrt (Math/sqrt num)]
    (= 0.0 (- sqrt (int sqrt)))))

(time
 (let [sigma (sigma-sieve 1000000)]
   (->> (range 1 1000000)
        (map #(- (aget ^ints sigma %) (* 2 %)))
        (filter (partial < 0))
        (filter quadratic)
        count)))

(let [sigma (sigma-sieve 50)]
  (dotimes [i 50] (println i (is-prime? i) (aget sigma i))))

(time 
 (let [sigma (sigma-sieve 1000000)]
   (count (filter neg-int? sigma))))


(defn sigma-1
  "https://en.wikipedia.org/wiki/Divisor_function"
  [num]
  )

