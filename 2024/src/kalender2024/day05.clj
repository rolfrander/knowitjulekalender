(ns kalender2024.day05
  (:require [clojure.string :as str]
            [rolfrander.puzzle-lib :as puzzle]))

(def data-candidates (slurp "https://julekalender-backend.knowit.no/challenges/2024-05/files/kandidater.txt?disposition=inline"))
(def data-votes (slurp "https://julekalender-backend.knowit.no/challenges/2024-05/files/stater.txt?disposition=inline"))

(defn parse-candidates [d]
  (->> (str/split-lines d)
       (map #(str/split % #" - "))
       (map second)
       vec))

(defn parse-line [l]
  (let [[state electors _k1 k1 _k2 k2 _k3 k3 _k4 k4] (map puzzle/str->long (re-seq #"[0-9]+" l))]
    {:state state
     :electors electors
     :votes [k1 k2 k3 k4]
     }))

(defn parse-results [d]
  (->> (str/split-lines d)
       (map parse-line)))

(defn calc-el-pr-candidate [state]
  (let [electors (:electors state)
        votes (:votes state)
        total-votes (float (apply + votes))]
    (->> votes
         (map #(* electors (/ % total-votes)))
         vec)))

(defn adjust-electors [state]
  (let [precise-elec-votes (calc-el-pr-candidate state)
        rounded-elec-votes (vec (map #(int (Math/floor %)) precise-elec-votes))
        rest-elec-votes (vec (map #(- %1 %2)
                                  precise-elec-votes
                                  rounded-elec-votes))
        remaining-electors (- (:electors state) (apply + rounded-elec-votes))
        sorted-candidates (sort-by rest-elec-votes > [0 1 2 3])
        extra-votes (concat (repeat remaining-electors 1) (repeat 0))]
    (assoc state
           :adjusted-votes (reduce #(update %1 (first %2) (partial + (second %2)))
                                   rounded-elec-votes
                                   (map vector sorted-candidates extra-votes)))))


(defn key-max [elements]
  (loop [[e & other] elements
         mx Long/MIN_VALUE
         i 0
         idx -1]
    (if (nil? e)
      idx
      (if (> e mx)
        (recur other e (inc i) i)
        (recur other mx (inc i) idx)))))

(let [cand (parse-candidates data-candidates)
      res (->> (parse-results data-votes)
               (map adjust-electors))
      totals (vec (reduce (partial map +) (map :adjusted-votes res)))
      winner (key-max totals)
      ]
  (format "%s - %s"
          (cand winner)
          (totals winner))
  )
;;=> "Donisse Tramp - 131"

