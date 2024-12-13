(ns kalender2024.day12
  (:require [java-time :as time]))

(defn is-sunday? [date]
  (= (time/day-of-week date) java.time.DayOfWeek/SUNDAY))

(defn is-advent-or-july? [date]
  (let [month (time/month date)
        year (time/year date)
        christmas (time/local-date year 12 25)
        ]
    (or (and (time/after? date (time/minus christmas (time/days 29))) 
             (time/before? date christmas))
        (= month java.time.Month/JULY))))

;(is-advent-or-july? (time/local-date 2023 11 28))

(def ^:dynamic *debug* false)

(defn bookmarks-until [start-date end-date]
  (loop [date start-date
         prev 0
         curr 0
         problem-counter 1
         bookmarks 0]
    (if (time/after? date end-date)
      bookmarks
      (let [is-problem (and (is-sunday? date)
                            (or (= problem-counter 3)
                                (is-advent-or-july? date)))
            next-problem-counter (if (is-sunday? date)
                                   (if is-problem 1 (inc problem-counter))
                                   problem-counter)
            tomorrow (time/plus date (time/days 1))
            new-bookmarks (cond
                            ;; feilsøkingsdag
                            is-problem 0
                            ;; dag etter feilsøking
                            (= curr 0) 1
                            :else
                            (+ prev curr))
            debug (and *debug* (or (time/after?  date (time/minus end-date (time/days 45)))
                                   (time/before? date (time/plus start-date (time/days 45)))))]
        (when debug (println "Antall bokmerker" (str date) new-bookmarks "akkumulert" bookmarks "problem" is-problem))
        ;(when (is-sunday? date)
        ;  (println date (if (is-sunday? date) (str "problem? " is-problem) "") "problem-cnt" problem-counter "cur bookmarks" curr))
        (recur tomorrow
               curr new-bookmarks next-problem-counter
               (+ bookmarks new-bookmarks))))))

(binding [*debug* true]
  (let [start-date (time/local-date 2020 4 1)
        end-date (time/local-date 2024 12 12)]
    (bookmarks-until start-date end-date)))

https://github.com/nrepl/nrepl/issues/285