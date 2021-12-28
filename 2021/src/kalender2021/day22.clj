(ns kalender2021.day22 
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.data.priority-map :refer [priority-map]]))

(def testdata "🎅🎅⛄🎄✨⛄⛄🎅🎄✨✨⛄🎅🎄🎄✨")

(def data (slurp "https://julekalender-backend.knowit.no/rails/active_storage/blobs/redirect/eyJfcmFpbHMiOnsibWVzc2FnZSI6IkJBaHBBdHNDIiwiZXhwIjpudWxsLCJwdXIiOiJibG9iX2lkIn19--714ad1051b4004fa4b99b3d06fda2801cc8abb2c/boards.txt?disposition=inline"))

(defn print-cube [v]
  (doseq [i (range 0 16)]
    (print (v i))
    (when (= (mod i 4) 3) (newline))))

(defn print-cubes [vs]
  (doseq [row [0 1 2 3]]
    (doseq [v vs]
      (doseq [c [0 1 2 3]]
        (print (v (+ c (* row 4)))))
      (print "  "))
    (newline)))

(defn parse-line [d]
  (vec
   (for [c (seq d)
         :let [i (int c)]
         :when (not= 55356 i)]
     (case i
       57221 \J
       9924  \S
       57220 \G
       10024 \T))))

(def circle [0 1 2 3])

(defn rot-horiz [cube row rot]
  (mapv cube
        (for [r circle
              c (if (= r row) rot circle)]
          (+ c (* r 4)))))

(defn rot-left [cube row]
  (rot-horiz cube row [1 2 3 0]))

(defn rot-right [cube row]
  (rot-horiz cube row [3 0 1 2]))

(defn rot-vert [cube col rot]
  (mapv cube
        (for [r circle
              c circle
              :let [r2 (if (= c col) (rot r) r)]]
          (+ c (* r2 4)))))

(defn rot-up [cube col]
  (rot-vert cube col [1 2 3 0]))

(defn rot-down [cube col]
  (rot-vert cube col [3 0 1 2]))

(def solution [\J \J \J \J \S \S \S \S \T \T \T \T \G \G \G \G])

(def expected-row {\J 0
                   \S 1
                   \T 2
                   \G 3})

(defn solved? [cube]
  (= cube solution))

(def all-transforms (for [f [rot-up rot-down rot-left rot-right]
                          i circle]
                      #(f % i)))

(def vert-transforms (for [f [rot-up rot-down]
                           i circle]
                      #(f % i)))

(defn next-generation [cubes]
  (mapcat (fn [tx] (map tx cubes)) all-transforms))

(defn solved-in-n [previous-solutions]
  (let [n (count previous-solutions)
        new-solutions (next-generation (previous-solutions (dec n)))]
       (->> (reduce (fn [res prev]
                      (remove prev res))
                    new-solutions
                    (reverse previous-solutions))
            (into #{})
            (conj previous-solutions))))

(def solutions (iterate solved-in-n [#{solution}]))

(count (peek (nth solutions 7)))

(defn find-precomputed [cube max]
  (let [s (nth solutions max)]
    (some #(when (contains? (s %) cube) %) (range (inc max)))))

;(find-precomputed (parse-line testdata) 5)

(defn min-moves-heuristic [cube]
  (reduce + (for [i (range 16)
                  :let [moves (Math/abs (- (expected-row (cube i))
                                           (quot i 4)))]]
              (if (= moves 3) 1 moves))))

(defn number-of-errors-heuristic [cube]
  (quot (->> (map = cube solution)
             (remove false?)
             count)
        3))

(let [cube [\J \J \J \G
            \S \S \S \J
            \T \G \T \S
            \T \G \T \G]]
  (number-of-errors-heuristic cube))

(defn solve [data max]
  (loop [queue (vector data)
         seen #{}
         i 0]
    (let [p (some #(find-precomputed % 6) queue)]
      (cond
        p (+ i p)
        ;(some solved-in-3 queue) (+ i 3)
        ;(some solved-in-2 queue) (+ i 2)
        ;(some solved-in-1 queue) (+ i 1)
        ;(some solved? queue) i
        (>= i max) nil

        :else
        (recur (into #{} (remove seen (next-generation queue)))
               (into seen queue)
               (inc i))))))

(defn solve-a-star [d h]
  (let [count-transforms (fn [came-from current]
                           (loop [i 0
                                  current current]
                             (if (not (contains? came-from current))
                               i
                               (recur (inc i) (came-from current)))))]
    (loop [;open-set (conj #{} d) ; openSet := {start}
           came-from {}          ; cameFrom := an empty map
           g-score (assoc {} d 0); gScore := map with default value of Infinity
           f-score (priority-map d (h d))] ; fScore := map with default value of Infinity
      (let [current (ffirst f-score)]                      ; current := the node in openSet having the lowest fScore value
        (cond (nil? current)      ; while openSet is not empty
              nil                    ; return failure

              ;(solved-in-3 current) (+ (count-transforms came-from current) 3)
              ;(solved-in-2 current) (+ (count-transforms came-from current) 2)
              ;(solved-in-1 current) (+ (count-transforms came-from current) 1)
              (solved? current)                              ; if current = goal
              (count-transforms came-from current)             ; return reconstruct_path(cameFrom, current)

              :else
              (let [f-score (pop f-score)             ; openSet.remove(current)
                    neighbours (map #(% current) all-transforms) ; for each neighbour of current
                    [came-from g-score f-score]
                    (reduce (fn [[came-from g-score f-score] n]
                              (let [tentative-g-score (inc (or (g-score current) 9999))] ; tentative_gScore := gScore[current] + d(current, neighbour)  (NB d er alltid = 1)
                                (if (< tentative-g-score (or (g-score n) 9999))          ; if tentative_gScore < gScore[neighbour]
                                  [(assoc came-from n current)                           ; cameFrom[neighbour] := current
                                   (assoc g-score n tentative-g-score)                   ; gScore[neighbour] := tentative_gScore
                                   (assoc f-score n (+ tentative-g-score (h n)))] ; fScore[neighbour] := tentative_gScore + h(neighbour)
                                  [came-from g-score f-score])))
                            [came-from g-score f-score]
                            neighbours)]
                (recur came-from g-score f-score)))))))

(solve (parse-line testdata) 6)
(solve-a-star (parse-line (first (string/split-lines data))) number-of-errors-heuristic)

(->> (string/split-lines data)
     (map parse-line)
     (map #(vector % (min-moves-heuristic %)))
     (sort-by second)
     )

(let [x [\J \G \T \J \S \J \G \S \S \T \J \T \G \T \S \G]]
  (print-cube x) (newline)
  (doseq [c (solve-a-star x min-moves-heuristic)]
    (print-cube c)
    (newline)))

(def tmp-solutions
  (for [d (->> (string/split-lines data)
               (map parse-line)
               (map #(assoc {}
                            :cube %
                            :min-moves (min-moves-heuristic %)
                            :num-err (number-of-errors-heuristic %)))
               (sort-by :min-moves)
               doall)]
    (let [s (assoc d :bfs (solve (:cube d) 6)
                   :a* (solve-a-star (:cube d) min-moves-heuristic))]
      (prn s)
      s)))

(last tmp-solutions)

(doseq [s (take 60 tmp-solutions)]
  (printf "%s;%s;%s;%s\n" (:bfs s)
          (:num-err s)
          (:min-moves s)
          (:a* s)))

(reduce + (map :bfs tmp-solutions))

(time (reduce (fn [[count sum] line]
                (if-let [s (solve-a-star line)]
                  [(inc count) (+ sum s)]
                  [count sum]))
              [0 0] ; count sum
              (string/split-lines data)))