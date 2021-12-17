(ns kalender2021.day13
  (:require [clojure.string :as string]))

(def data (string/split-lines (slurp "https://julekalender-backend.knowit.no/rails/active_storage/blobs/redirect/eyJfcmFpbHMiOnsibWVzc2FnZSI6IkJBaHBBcmtDIiwiZXhwIjpudWxsLCJwdXIiOiJibG9iX2lkIn19--084d36cb3409a1e265d498b4345d745b1356842c/moves.txt?disposition=inline")))

(def move {\N [0 -1]
           \S [0  1]
           \E [1  0]
           \W [-1 0]
           \I [0  0]})

(def startposition
  (->> 0
       (repeat 9) vec
       (repeat 9) vec))

(defn clamp [v min max]
  (cond (< v min) min
        (> v max) max
        :else v))

(defn do-move [[x y] direction]
  (let [[dx dy] (move direction)
        nx (clamp (+ x dx) 0 8)
        ny (clamp (+ y dy) 0 8)
        ]
    [nx ny]
    ))

(defn interpret-line [stacks ^String line]
  (loop [pos [4 4]
         i 0
         h 249]
    (if (or (= i (.length line))
            (= (- h (get-in stacks pos)) 1))
      (update-in stacks pos inc)
      (let [newpos (do-move pos (.charAt line i))]
        (recur (if (< (get-in stacks newpos) h) newpos pos)
               (inc i)
               (dec h))))))

(->> (reduce interpret-line startposition data)
     flatten
     (apply max))
