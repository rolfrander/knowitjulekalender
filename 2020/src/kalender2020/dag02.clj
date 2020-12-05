(ns dag02.core
  (:gen-class))

(set! *warn-on-reflection* true)

(defn ^booleans sieve [cnt]
  (let [result (boolean-array cnt true)]
    (aset-boolean result 0 false)
    (aset-boolean result 1 false)
    (doseq [i (range 2 (Math/sqrt cnt))]
      (when (aget result i)
        (doseq [j (range (* i i) cnt i)]
          (aset-boolean result j false))))
    result))

(let [primes ^booleans (sieve 5330000)]
  (defn find-closest-prime-sieve [look-for-number]
    (loop [i look-for-number]
      (if (aget primes i)
        i
        (recur (dec i))))))

(defn ^ints load-primes [filename]
  (ints (int-array
         (map #(Integer/parseInt %)
              (->> filename
                   slurp
                   (re-seq #"\d+"))))))

; liste av primtall fra https://primes.utm.edu/lists/small/millions/primes1.zip
(def primes (load-primes "primes1.txt"))

(defn find-closest-prime [look-for-number]
  (let [number (int look-for-number)
        just-above (loop [left 0
                          right (int (alength ^ints primes))]
                      (if (> (- right left) 10)
                        (let [middle (int (/ (+ left right) 2))
                              middle-value (int (aget ^ints primes middle))]
                          (cond
                            (< middle-value number) (recur (inc middle) right)
                            (> middle-value number) (recur left (dec middle))
                            :else middle))
                        right))]
    (loop [i just-above]
      (if (> (int (aget ^ints primes i)) number)
        (recur (dec i))
        (aget ^ints primes i)))))

(time (dotimes [i 100000] (find-closest-prime 8000000)))

(defn contains-7 [i]
  (clojure.string/includes? (str i) "7"))

(defn calc-gifts [upper-bound]
  (let [bound (int upper-bound)]
    (loop [giftno 0
           giftcount 0]
      (cond
        (>= giftno bound) giftcount
        (contains-7 giftno) (recur (int (+ 1 giftno (find-closest-prime giftno))) giftcount)
        :else (recur (inc giftno) (inc giftcount))))))

(time (calc-gifts 5433000))

(take (dec (int (java.lang.Math/sqrt 26))) (iterate inc 2))
(take-while (partial > 30) (iterate #(+ 3 %) (* 3 3)))

(doseq [x [2 3 4 5]
        y (take-while (partial > 5) (iterate inc x))]
  (prn [x y]))

(range 2 (Math/sqrt 24))
(range 9 20 3)
(last primes)

(time )


(time (dotimes [i 1000000] (find-closest-prime-sieve (+ i 10))))
(time (dotimes [i 1000000] (find-closest-prime (+ i 10))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


