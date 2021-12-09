(ns kalender2021.day08
  (:require [clojure.string :as string]))


(set! *warn-on-reflection* true)
(set! *unchecked-math*  :warn-on-boxed)

(def testdata "(1,1)
(6,6)
(3,2)
(6,1)
0
1
2")

(defn parse [data]
  (reduce (fn [[stops route] line]
            (if (= \( (.charAt line 0))
              [(conj stops (map #(Long/parseLong %) (re-seq #"[0-9]+" line)))
               route]
              [stops (conj route (Long/parseLong line))]))
          [[][]]
          (string/split-lines data)))

(defn drop-area [[x1 y1][x2 y2]]
  (for [x (range x1 x2 (if (> x1 x2) -1 1))
        y (range y1 y2 (if (> y1 y2) -1 1))]
    [x y]))

;(drop-area [1 1][6 6])
;(drop-area [6 6][3 2])

(def data (parse (slurp "https://julekalender-backend.knowit.no/rails/active_storage/blobs/redirect/eyJfcmFpbHMiOnsibWVzc2FnZSI6IkJBaHBBcEVDIiwiZXhwIjpudWxsLCJwdXIiOiJibG9iX2lkIn19--c58f01ade88c36b61eda8ecd198328de54f47160/input.txt?disposition=inline")))

(count (second data))

(let [[stops route] (parse 
                     data
                     ;testdata
                     )
      packets (make-array Long/TYPE 1000 1000)]
  (loop [result [1000 1000 0 0 0]
         first-stop (first route)
         [next-stop & route] (rest route)]
    (println first-stop)
    (if (nil? next-stop)
      result
      (recur (reduce (fn [result [x y]]
                       (let [v (aset packets x y (inc (aget packets x y)))
                             cur-max (nth result 4)]
                         (cond (< v cur-max) result
                               (> v cur-max) [x y x y v]
                               :else (let [[x1 y1 x2 y2 cur-max] result]
                                       [(min x x1) (min y y1) (max x x2) (max y y2) cur-max]))))
                     result
                     (drop-area (get stops first-stop)
                                (get stops next-stop)))
             next-stop
             route))))

(defn throw-packages [stops route]
  (let [packets (make-array Integer/TYPE 1000 1000)
        start (System/currentTimeMillis)]
    (loop [first-stop (first route)
           [next-stop & route] (rest route)
           cnt 1]
      (when (= 0 (mod cnt 20))
        (let [elapsed (- (System/currentTimeMillis) start)]
          (println elapsed cnt (count route) (/ (* elapsed (count route)) (* 60000.0 cnt)))))
      (if (nil? next-stop)
        packets
        (let [[x1 y1] (get stops first-stop)
              [x2 y2] (get stops next-stop)
              x (int (min x1 x2))
              y (int (min y1 y2))
              max-y (int (Math/abs (int (- y2 y1))))]
        ;(time)
          (dotimes [j (int (Math/abs (int (- x2 x1))))]
            (dotimes [i max-y]
              (aset-int packets (+ x j) (+ y i) (inc ^int (aget packets (+ x j) (+ y i))))))
          (recur next-stop
                 route
                 (inc cnt)))))))

(apply throw-packages data)