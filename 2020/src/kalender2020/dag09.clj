(ns kalender2020.dag09
  (:require [clojure.set :as set]
            [clojure.string :as string]))

(set! *unchecked-math* true)
(set! *warn-on-reflection* true)

(def testdata "FFFSF  
FSFFF  
FSFSF  
SFFSF  
FSFFF ")

(def data (slurp "https://julekalender-backend.knowit.no/challenges/9/attachments/elves.txt"))

(defn parse-data 
  "tar lang streng med F og S som input, returnerer vector-med-(bool-array). 
   Lager også en ramme av friske celler rundt hele brettet så vi slipper å teste
   for grenseverdier."
  [data]
  (let [add-empty-row (fn [board] (conj board (int-array (count (first board)) 0)))]
    (->> data
         string/split-lines
         (map (fn [line]
                (let [line (str line)
                      game-board-row (int-array (+ 2 (count line)) 0)] ; rad er 2 celler bredere enn input
                  (dotimes [i (count line)]
                    (if (= \S (.charAt line i))
                      (aset-int game-board-row (inc i) 1))) ; hopper over første celle (default = false)
                  game-board-row)))
       ; legge til tom rad foran
         add-empty-row
       ; konvertere til vector
         (into [])
       ; legge til tom rad bak
         add-empty-row)))

(defn display [grid]
  (doseq [row grid]
    (println (string/join (map #(if % \S \F) row)))))

(defn find-healthy-cells [grid]
  (->> (for [y (range 1 (dec (count grid)))
             x (range 1 (dec (count (get grid y))))]
         (if (= 0 (get-in grid [y x])) [x y]))
       (filter #(not (nil? %)))))

(defn count-infected-neighbours [^booleans grid ^long x ^long y]
  (cond-> 0
    (get-in grid [(dec y) x]) inc
    (get-in grid [(inc y) x]) inc
    (get-in grid [y (dec x)]) inc
    (get-in grid [y (inc x)]) inc)
  )

(defn count-infected-neighbours [^ints grid ^long x ^long y]
  (+ (get-in grid [(dec y) x])
     (get-in grid [(inc y) x])
     (get-in grid [y (dec x)])
     (get-in grid [y (inc x)])))

(defn find-healthy-cells-being-infected
  "returnerer liste av [x y] for friske celler med minst 2 syke naboer"
  [grid]
  (filter #(<= 2 (count-infected-neighbours grid (first %) (second %)))
          (find-healthy-cells grid)))

(defn evolve
  "kjører spill i henhold til regler, returnerer antall endringer gjort"
  [grid]
  (let [infect (doall (find-healthy-cells-being-infected grid))] ; can not be lazy
    (reduce (fn [count [x y]]
              (aset-int (get grid y) x 1)
              (inc count))
            0
            infect)))

(time 
 (let [d (parse-data data)]
   (inc (first (drop-while (fn [_] (> (evolve d) 0)) (range))))))

;;; **************************

(defn parse-data-as-set
  [data]
  (->> data
       string/split-lines
       (map-indexed (fn [y line]
                      (map-indexed (fn [x char]
                                     (if (= char \F) [x y]
                                         nil))
                                   line)))
       (mapcat identity)
       (filter #(not (nil? %)))
       (into #{})))

(defn neighbours [[^long x ^long y] ^long maxdim]
  (into #{} (filter (fn [[^long x ^long y]] (and (> x -1) (> y -1) (< x maxdim) (< y maxdim)))
                    [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])))

(defn infected? [grid position maxdim]
  (<= 2 (count (set/difference (neighbours position maxdim) grid))))

(defn evaluate-as-set [data]
 (let [g (parse-data-as-set data)
       maxdim (long (inc (apply max (map first g))))]
   (loop [grid g
          i 1]
     (let [evolved (into #{} (filter #(not (infected? grid % maxdim))
                                     grid))]
       (if (= (count grid) (count evolved))
         i
         (recur evolved (inc i))))
     )))

(time (evaluate-as-set data))

(neighbours [0 0] 5)
(infected? (parse-data-as-set testdata) [0 4] 5)

(doseq [x (range 5) y (range 5)] (print x y))

(defn pp-grid [grid dim]
  (doseq [y (range dim)
          x (range dim)]
    (do 
      (when (= x 0) (println))
      (if (contains? grid [x y])
        (print  [x y])
        (print "     "))
      ))
  (println) (flush))

(let [grid (parse-data-as-set testdata)
      dim (inc (apply max (map first grid)))]
  (pp-grid grid dim)
  (pp-grid (into #{} (filter #(not (infected? grid % dim)) grid)) dim)
  nil)
;; => (
;; [0 0] [1 0] [2 0] 
;; [0 1]       [2 1]             [4 1]
;; 
;;             [2 3])


