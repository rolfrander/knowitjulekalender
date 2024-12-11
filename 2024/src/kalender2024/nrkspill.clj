(ns src.kalender2024.nrkspill
  (:require [clojure.string :as str]
            [rolfrander.puzzle-lib :as puzzle]
            [clojure.set :as set]))

(set! *warn-on-reflection* true)

(def brett "bbgbgor
bgggrbr
bbogorr
gogbbrb
orobgog
rbbobbo
obgrobo
rbrroob
gbbrobg")

(def brett "borgogr
bbooogb
rrrrgob
bbboobg
oboobbr
bgobgoo
bbogoor
rbbgrob
gbgobgo")

(defn parse-brett [brett-string]
  (->> brett-string
       str/split-lines
       (apply map list)
       (map reverse)
       (map vec)
       vec))

(defn print-brett [brett]
  (when (some seq brett)
    (print-brett (map rest brett))
    (doseq [c brett]
      (print (case (first c)
               \b "🟦"
               \g "🟩"
               \o "🟨"
               \r "🟥"
               "🔳")))
    (println)))

(def n (puzzle/neighbours-fn :sq-4 :ignore :max-dim [6 8]))

(defn finn-sammenhengende-omr [brett [x y :as pos]]
  (let [gc (fn [pos] (get-in brett pos))
        color (gc pos)]
    (letfn [(fill [area [x y :as pos]]
              (cond (contains? area pos) area
                    (= color (gc pos)) (reduce fill (conj area pos) (n pos))
                    :else area))]
      (fill #{} pos))
    ))


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

(defn search [brett max-iter moves]
  ;(println "moves" max-iter moves)
  ;(print-brett brett)
  (cond (= 0 max-iter) nil
        (not (some seq brett)) moves
        :else (some #(search (plasser brett %)
                             (dec max-iter)
                             (conj moves %))
                    (finn-alle-relevante-startpos brett))))

;; ***********************************************************************

(defn coord [x y] (+ (* x 9) y))

(defn parse-boa+rd-a [board-string]
  (let [board (make-array Character/TYPE (* 7 9))
        rows (str/split-lines board-string)]
    (doseq [[y row] (partition 2 (interleave (range) rows))
            [x c] (partition 2 (interleave (range) row))]
      (aset-char board (coord x (- 8 y)) c))
    board))

(defn blank-brett-a [] 
  (let [a (make-array Character/TYPE (* 7 9))]
    (doseq [i (range (* 7 9))]
      (aset-char a i \space))
    a))

(defn print-brett-a [^chars brett]
  (doseq [y (range 8 -1 -1)
          x (range 0 7)]
    (print (case (aget brett (coord x y))
             \b "🟦"
             \g "🟩"
             \o "🟨"
             \r "🟥"
             "🔳"))
    (when (= x 6) (println))))

(defn get-brett-a [^chars brett x y]
  (aget brett (coord x y)))

(defn make-mask [] (make-array Boolean/TYPE (* 7 9)))

(defn get-mask [^booleans brett ^long x ^long y] (aget brett (coord x y)))

(defn set-mask [^booleans brett ^long x ^long y v] (aset-boolean brett (coord x y) (boolean v)))

(defn print-with-mask-a [^chars brett ^booleans mask]
  (doseq [y (range 8 -1 -1)
          x (range 0 7)]
    (print (case (if (get-mask mask x y)
                   \space
                   (aget brett (coord x y)))
             \b "🟦"
             \g "🟩"
             \o "🟨"
             \r "🟥"
             "🔳"))
    (when (= x 6) (println))))

(defn finn-sammenhengende-omr-a [^chars brett ^booleans v [x y :as pos]]
  (let [gc (fn [^long x ^long y] (get-brett-a brett x y))
        n (fn [^long x ^long y] 
            (cond (and (= x 0) (= y 0)) [[1 0] [0 1]]
                  (and (= x 0) (= y 8)) [[1 8] [0 7]]
                  (and (= x 6) (= y 0)) [[5 0] [6 1]]
                  (and (= x 6) (= y 8)) [[5 8] [6 7]]
                  (= x 0) [[0 (unchecked-inc y)] [1 y] [0 (unchecked-dec y)]]
                  (= x 6) [[6 (unchecked-inc y)] [5 y] [6 (unchecked-dec y)]]
                  (= y 0) [[(unchecked-inc x) 0] [x 1] [(unchecked-dec x) 0]]
                  (= y 8) [[(unchecked-inc x) 8] [x 7] [(unchecked-dec x) 8]]
                  :else [[(unchecked-inc x) y] [(unchecked-dec x) y]
                         [x (unchecked-inc y)] [x (unchecked-dec y)]]))
        color (gc x y)]
    (letfn [(fill [area [x y]]
              (cond (get-mask area x y) area
                    (= color (gc x y)) (do (set-mask area x y true)
                                           (reduce fill area (n x y)))
                    :else area))]
      (fill v pos))))


(let [b (parse-board-a brett)
      m (finn-sammenhengende-omr-a b (make-mask) [6 8])]
  (print-with-mask-a b m))

(defn finn-alle-relevante-startpos-a [^chars brett]
  (loop [visited (make-mask)
         startpos []
         x 0
         y 0]

    (cond (> x 6) startpos

          (or (> y 8) (= \space (get-brett-a brett x y)))
          (recur visited startpos (unchecked-inc x) 0)

          (get-mask visited x y) 
          (recur visited startpos x (unchecked-inc y))

          :else
          (recur (finn-sammenhengende-omr-a brett visited [x y])
                 (conj startpos [x y])
                 x
                 (unchecked-inc y)))))

(defn flytt-a [^chars old ^booleans fjernes]
  (let [new (blank-brett-a)]
    (doseq [x (range 0 7)]
      (loop [y-old 0
             y-new 0]
        (when-not (or (> y-old 8)
                      (= \space (get-brett-a old x y-old)))
          (if (get-mask fjernes x y-old)
            (recur (unchecked-inc y-old) y-new)
            (do (aset-char new (coord x y-new) (aget old (coord x y-old)))
                (recur (unchecked-inc y-old)
                       (unchecked-inc y-new)))))))
    new))

(defn plasser-a [^booleans brett pos]
  (flytt-a brett
           (finn-sammenhengende-omr-a brett (make-mask) pos)))

(defn search-a [^chars brett max-iter moves]
  ;(println "moves" max-iter moves)
  ;(print-brett brett)
  (cond (= 0 max-iter) nil
        (not (some #(not= \space (get-brett-a brett % 0))
                   (range 7))) ['foobar moves]
        :else (some #(search-a (plasser-a brett %)
                               (dec max-iter)
                               (conj moves %))
                    (finn-alle-relevante-startpos-a brett))))

(def start-brett (parse-brett brett))
(def start-brett-a (parse-board-a brett))

(print-brett start-brett)
(print-brett-a start-brett-a)
(print-brett-a (-> start-brett-a 
                   (plasser-a [1 0])
                   (plasser-a [5 1])
                   (plasser-a [3 5])
                   ))

(time (search start-brett 4 []))

(time (search-a start-brett-a 2 []))

(time (finn-alle-relevante-startpos start-brett))

(time (finn-alle-relevante-startpos-a start-brett-a))

