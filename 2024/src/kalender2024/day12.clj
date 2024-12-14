(ns kalender2024.day12
  (:require [java-time :as time]))

(defn is-advent-or-july? [date]
  (let [month (time/month date)
        year (time/year date)
        christmas (time/local-date year 12 24)
        start-advent (time/minus christmas (time/days 27))]
    (or (time/<= start-advent date christmas)
        (= month java.time.Month/JULY))))

(def fibs (->> (iterate (fn [[a b]] [b (+ a b)]) [0 1])
               (map first)
               (take 23)
               vec))

(defn bookmarks-until [start-date end-date]
  (loop [date start-date
         bookmarks 0
         problem-counter 0
         i 0]
    (if (time/after? date end-date)
      bookmarks
      (let [is-problem (and (time/sunday? date)
                            (or (>= problem-counter 2)
                                (is-advent-or-july? date)))
            
            next-problem-counter (cond is-problem 0
                                       (time/sunday? date) (inc problem-counter)
                                       :else problem-counter)
            
            next-i (if is-problem 0 (inc i))]
        
        (recur (time/plus date (time/days 1))
               (+ bookmarks (fibs next-i))
               next-problem-counter
               next-i)))))

(let [start-date (time/local-date 2020 4 1)
      end-date (time/local-date 2024 12 12)]
  (bookmarks-until start-date end-date))

