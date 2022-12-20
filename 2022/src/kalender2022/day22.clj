(ns kalender2022.day22 
  (:require [clojure.string :as str]))

(def production {-1 {"Mye regn" 120
                     "Lite eller ingen regn" 100}
                 0  {"Mye regn" 80
                     "Lite eller ingen regn" 60}
                 1  {"Mye regn" 40}})

(def testgraf (slurp "https://julekalender-backend.knowit.no/challenges/2022-22/files/vannstand_eksempel.txt"))
(def testlogg (slurp "https://julekalender-backend.knowit.no/challenges/2022-22/files/vaer_eksempel.csv"))
(def graf (slurp "https://julekalender-backend.knowit.no/challenges/2022-22/files/vannstand.txt"))
(def logg (slurp "https://julekalender-backend.knowit.no/challenges/2022-22/files/vaer.csv?"))

(defn parse-logg [input]
  (->> (str/split-lines input)
       (drop 1)
       (map #(str/split % #","))
       (map second)))

(defn parse-graf [input]
  (->> (str/split-lines input)
       (apply map str)
       (map #(count (first (str/split % #"[^ ]" 2))))
       (partition 2 1)
       (map (partial apply -))))

(->> (map #(get-in production [%1 %2])
          (parse-graf graf)
          (parse-logg logg))
     (reduce +)
     (* 1000))
