(ns kalender2021.day18
  (:require [clojure.string :as string]))


;;; grunnleggende formler
(defn collatz [[prev _ignore]]
  [(if (even? prev)
     (quot prev 2)
     (inc (* prev 3)))
   0])

(defn niklatz [[prev cnt]]
  (let [is-niklatz (= (mod prev 37) 0)]
    (if (and (not is-niklatz)
             (= cnt 0))
      (collatz [prev cnt])
      [(if (even? prev)
         (inc (* prev 3))
         (quot prev 2))
       (if is-niklatz 2 (dec cnt))])))

;;; rett frem iterativ løsning
(defn seq-len [seq-fn start]
  (loop [cur (seq-fn [start 0])
         i 2]
    (if (= (first cur) 1)
      i
      (recur (seq-fn cur) (inc i)))))

(defn seq-sum [seq-fn start]
  (loop [cur (seq-fn [start 0])
         sum start]
    (let [sum (+ (first cur) sum)]
      (if (= (first cur) 1)
        sum
        (recur (seq-fn cur) sum)))
    ))

(defn simple-solution [max]
  (->> (range 1 (inc max))
       (keep #(if (not= (seq-len niklatz %)
                        (seq-len collatz %)) % nil))
       (map (partial seq-sum niklatz))
       (reduce +)))

;;; løsning som tar vare på tidligere resultater

(def seq-len-mem
  (let [mem (atom {})]
    (fn [seq-fn start]
      (if (nil? seq-fn)
        @mem
        (let [m (if (contains? @mem seq-fn)
                  (@mem seq-fn)
                  (do (swap! mem assoc seq-fn {})
                      (get @mem seq-fn)))

              res (loop [cur (seq-fn [start 0])
                         i 2]
                    (if (= (first cur) 1)
                      i
                      (if (and (= (second cur) 0) (contains? m (first cur)))
                        (+ (dec i) (get m (first cur)))
                        (recur (seq-fn cur) (inc i)))))]
          
          (swap! mem assoc-in [seq-fn start] res)
          res)))))

(seq-len-mem nil 37)

(keep #(let [a (seq-len niklatz %)
             b (seq-len-mem niklatz %)]
         (if (not= a b)
           [% a b]
           nil)) (range 1 1500))

(def seq-sum-mem 
  (let [mem (atom {})]
    (fn [seq-fn start]
      (let [m (if (contains? @mem seq-fn)
                (@mem seq-fn)
                (do (swap! mem assoc seq-fn {})
                    (get @mem seq-fn)))
            res (loop [cur (seq-fn [start 0])
                       sum start]
                  (if (and (= (second cur) 0) (contains? m (first cur)))
                    (+ sum (m (first cur)))
                    (let [sum (+ (first cur) sum)]
                      (if (= (first cur) 1)
                        sum
                        (recur (seq-fn cur) sum)))))]
        
        (swap! mem assoc-in [seq-fn start] res)
        res))))

(defn memoized-solution [max]
  (->> (range 1 (inc max))
       (keep #(if (not= (seq-len-mem niklatz %)
                        (seq-len-mem collatz %)) % nil))
       (map (partial seq-sum-mem niklatz))
       (reduce +)))

(defn memoized-parallell-solution [max]
  (->> (range 1 (inc max))
       (pmap #(if (not= (seq-len-mem niklatz %)
                        (seq-len-mem collatz %)) % nil))
       (keep identity)
       (pmap (partial seq-sum-mem niklatz))
       (reduce +)))

;;; beregne begge serier på en gang

(defn count-and-sum [fn1 fn2 start]
  (loop [cur1 (fn1 [start 0])
         cur2 (fn2 [start 0])
         sum2 start]
    (if (= (first cur1) (first cur2) 1)
      0
      (let [sum2 (+ (first cur2) sum2)]
        (if (= (first cur2) 1)
          sum2
          (if (<= (first cur1) 1)
            (recur [0 0] (fn2 cur2) sum2)
            (recur (fn1 cur1) (fn2 cur2) sum2)))))))

(count-and-sum collatz niklatz 1)

(defn combined-solution [max]
  (->> (range 1 (inc max))
       (map (partial count-and-sum collatz niklatz))
       (filter (partial not= 0))
       (reduce +)))

;;; tester
(time (simple-solution 1000000)) ; Elapsed time: 132526.5723 msecs
(time (memoized-solution 1000000)); Elapsed time: 23094.5646 msecs
(time (memoized-parallell-solution 1000000)) ; Elapsed time: 24591.2428 msecs


(->> (iterate niklatz [148 0])
     (take 20)
     (map first)
     (string/join " -> "))
;; => "148 -> 445 -> 222 -> 667 -> 333 -> 166 -> 499 -> 249 -> 748 -> 374 -> 187 -> 562 -> 281 -> 844 -> 422 -> 211 -> 634 -> 317 -> 952 -> 476"
;;     148 -> 445 -> 222 -> 667 -> 333 -> 166 -> 499 -> 249 -> 748 -> 374 -> 187 -> 562 -> 281 -> 844 -> 422 -> 211 -> 634 -> 317 -> 952 -> 476 -> 238 -> 119 -> 358 -> 179 -> 538 -> 269 -> 808 -> 404 -> 202 -> 101 -> 304 -> 152 -> 76 -> 38 -> 19 -> 58 -> 29 -> 88 -> 44 -> 22 -> 11 -> 34 -> 17 -> 52 -> 26 -> 13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1