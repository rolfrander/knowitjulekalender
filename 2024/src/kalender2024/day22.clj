(ns kalender2024.day22
  (:require [clojure.string :as str]
            [rolfrander.puzzle-lib :as puzzle]
            [clojure.set :as set]))

(def data (slurp "https://julekalender-backend.knowit.no/challenges/2024-22/files/stekebrett.txt?disposition=inline"))

(defn parse-brett [brett-string]
  (->> brett-string
       str/split-lines
       (map #(map {"🧸" \b
                   "🤎" \r
                   "🦌" \o
                   "🎄" \g} (re-seq #"."  %)))
       (apply map list)
       (map reverse)
       (map vec)
       vec))

(defn parse [in]
  (->> (str/split in #"\n\n")
       (map parse-brett)))

(defn print-brett [brett]
  (when (some seq brett)
    (print-brett (map rest brett))
    (doseq [c brett]
      (print (case (first c)
               \b "🧸"
               \g "🎄"
               \o "🦌"
               \r "🤎"
               "❌")))
    (println)))

(defn print-brett-ascii [brett]
  (when (some seq brett)
    (print-brett-ascii (map rest brett))
    (doseq [c brett]
      (print (case (first c)
               nil " "
               (first c))))
    (println)))

(def n (puzzle/neighbours-fn :sq-4 :ignore :max-dim [6 8]))

(defn finn-sammenhengende-omr [brett [x y :as pos]]
  (let [gc (fn [pos] (get-in brett pos))
        color (gc pos)]
    (letfn [(fill [area [x y :as pos]]
              (cond (contains? area pos) area
                    (= color (gc pos)) (reduce fill (conj area pos) (n pos))
                    :else area))]
      (fill #{} pos))))


(defn finn-alle-relevante-startpos [brett]
  (->> (for [x (range 7)
             y (range 9)]
         [x y])
       (reduce (fn [[ps visited] pos]
                 (if (or (nil? (get-in brett pos))
                         (contains? visited pos))
                   [ps visited]
                   (let [fill (finn-sammenhengende-omr brett pos)]
                     [(conj ps [pos (count fill)]) (set/union visited fill)])))
               [[] #{}])
       first
       (sort-by second #(> %1 %2))
       (map first)))

(defn flytt [brett fjernes]
  (mapv (fn [x kolonne]
          (filterv some?
                   (map-indexed (fn [y celle]
                                  (when-not (contains? fjernes [x y])
                                    celle))
                                kolonne)))
        (range) brett))

(defn plasser [brett pos]
  (flytt brett
         (finn-sammenhengende-omr brett pos)))

(defn search-a-star [brett result]
  (puzzle/a-star brett                        ; start
                 #(not (some seq %))          ; goal
                 #(bit-shift-right (apply + (map count %)) 2) ; heuristic
                 finn-alle-relevante-startpos ; paths
                 plasser                      ; neighbour
                 (constantly 1)               ; dist
                 result                       ; result-type
                 ))

(def brett (parse data))

(apply + (map #(search-a-star % :dist) brett))

