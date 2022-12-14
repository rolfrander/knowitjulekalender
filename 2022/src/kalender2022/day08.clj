(ns kalender2022.day08 
  (:require [clojure.string :as string]))

(def input (slurp "https://julekalender-backend.knowit.no/challenges/2022-08/files/data.txt"))

(defn debug [msg data]
  (println msg data)
  data)

(defn sign [x]
  (cond (< x 0) -1
        (> x 0) 1
        :else 0))

(defn compare-by-angle-to-x
  [[x y]]
  "returns a function taking parameters p1, p2 comparing the angle between vectors (p->p1) and
   x-axis to the angle between vectors (p->p2) and x-axis"
  (fn [[x1 y1] [x2 y2]]
    
    (let [delta-y1 (- y1 y)
          delta-x1 (- x1 x)
          delta-y2 (- y2 y)
          delta-x2 (- x2 x)
          diff (if (not= (sign delta-x1) (sign delta-x2)) ;; p1 and p2 in different quadrants
                 (- delta-x2 delta-x1)
                 (- (/ delta-y1 delta-x1)
                    (/ delta-y2 delta-x2)))]
      (if (= diff 0)
        (< (+ (- y1 y) (- x1 x))
           (+ (- y2 y) (- x2 x)))
        (< diff 0)))))

;((compare-by-angle-to-x [5 5])
; [7 7]
; [5 6])


(defn sort-points [points]
  (let [p0 (reduce (fn [[x y] [x1 y1]]
                     (cond (> y y1) [x1 y1]
                           (< y y1) [x y]
                           (< x x1) [x y]
                           :else [x1 y1]))
                   points)
        cmp (compare-by-angle-to-x p0)]
    (cons p0 (sort-by identity cmp (remove (partial = p0) points)))
    ))

(def testdata (sort-points [[10 7]
                            [5 7]
                            [7 10]
                            [9 12]
                            [15 6]
                            [9 4]
                            [8 9]]))


(defn parse [input]
  (->> (string/split-lines input)
       (map #(string/split % #" "))
       (map (fn [[x y]] [(Long/parseLong x) (Long/parseLong y)]))))

(def data (sort-points (parse input)))

(defn ccw [[x1 y1][x2 y2][x3 y3]]
  (- (* (- x2 x1) (- y3 y1))
     (* (- y2 y1) (- x3 x1))))

(defn area-polygon [points]
  (let [a2 (->> points
                (partition 2 1 points)
                (map (fn [ [[x1 y1] [x2 y2]]]
                       (* (+ y1 y2)
                          (- x1 x2))))
                (reduce +)
                )]
    (/ a2 2)))

;(area-polygon [[1 1] [4 1] [3 3] [1 3]])

(loop [[p & rest-points :as points] data
       stack '()]
  (if (nil? p)
    (Math/abs (area-polygon stack))
    (let [[p1 p2 & r] stack]
      (if (and (not (nil? p2))
               (<= (ccw p2 p1 p) 0))
        (recur points (rest stack))
        (recur rest-points (cons p stack))))))

