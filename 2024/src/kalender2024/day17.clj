(ns kalender2024.day17
  (:require
   [clojure.string :as str]
   [rolfrander.puzzle-lib :as puzzle]))

(def data (slurp "https://julekalender-backend.knowit.no/challenges/2024-17/files/tall.txt?disposition=inline"))

(def tall (map puzzle/str->long (str/split-lines data)))

(defn get-digits [^long n]
  (if (< n 10)
    [n]
    (conj (get-digits (quot n 10)) (mod n 10))))

(defn factorize [^long n]
  (loop [[p & primes] (filter puzzle/is-prime? (range 2 (int (Math/sqrt n))))
         remainder n
         factors []]
    ;(println p remainder factors)
    (if (nil? p)
      factors
      (if (= (mod remainder p) 0)
        (recur (cons p primes) (quot remainder p) (conj factors p))
        (recur primes remainder factors)))))

;(sort (mapcat get-digits (factorize 16368)))

(defn index-differ [f]
  (first (reduce (fn [[idxs idx prev] cur]
                   [(if (or (= idx 1) (not= prev cur))
                      (conj idxs idx)
                      idxs)
                    (inc idx)
                    cur])
                 [[] 0 -1] f)))

(defn combine-factors [digits-of-n factors]
  ;(println n factors)
  (let [factor-digits (vec (sort (mapcat get-digits factors)))]
    (cond (< (count factor-digits) (count digits-of-n))
          nil ; fewer digits in the factors than in the original number, can't be a vampire

          (= digits-of-n factor-digits) ; assumes both lists are sorted
          factors ; found a match

          (<= (count factors) 2) ; don't bother if there are only two
          nil

          :else ; try all combinations of combining two factors, recur, 
          (let [factor-combinations (->> (for [rest-factors (iterate rest (index-differ factors))
                                               :while (seq rest-factors)
                                               b (rest rest-factors)
                                               :let [a (first rest-factors)]]
                                           (->> (map-indexed #(cond (= %1 a) (* (factors a) (factors b))
                                                                    (= %1 b) nil
                                                                    :else %2)
                                                             factors)
                                                (remove nil?)
                                                sort
                                                vec))
                                         distinct)]
            (some (partial combine-factors digits-of-n) factor-combinations)))))

;(combine-factors 16368 (vec (sort (get-digits 16368))) (factorize 16368))

(defn pseudo-vampire? [n]
  (let [d (vec (sort (get-digits n)))]
    (boolean (combine-factors d (factorize n)))))

(pseudo-vampire? 249480)

(vec (sort (mapcat get-digits (factorize 249480))))

(for [rest-factors (iterate rest (index-differ (factorize 249480)))
      :while (seq rest-factors)
      b (rest rest-factors)
      :let [a (first rest-factors)]]
  [a b])

(with-open [out (clojure.java.io/writer "debug.txt")]
  (binding [*out* out]
    (doseq [t tall]
      (println t (pseudo-vampire? t))
      (flush))))

(first (drop 101 tall))
(combine-factors (get-digits 249480) (factorize 249480))

(let [factors (factorize 249480)
      index-differ (fn [f] (first (reduce (fn [[idxs idx prev] cur]
                                            [(if (not= prev cur) (conj idxs idx) idxs)
                                             (inc idx)
                                             cur])
                                          [[] 0 -1] f)))]
  (index-differ factors))

(time
 (->> tall
      (filter pseudo-vampire?)
      (reduce +)
      ))
;;=> 