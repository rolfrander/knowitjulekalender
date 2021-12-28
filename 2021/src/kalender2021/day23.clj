(ns kalender2021.day23
  (:require [kalender2021.day20 :as day20]
            [kalender2021.core :as core]))

(let [maze (day20/parse day20/data)
      width (count (first maze))
      height (count maze)
      nodeset (for [row (range height)
                    col (range width)]
                [row col])
      directions {:n [-1 0]
                  :e [0 1]
                  :s [1 0]
                  :w [0 -1]}
      outside-maze (fn [[r c]] (or (< r 0) (< c 0) (>= r height) (>= c width)))
      neighbours (fn [pos]
                   (->> [:n :e :s :w]
                        (remove (partial day20/has-wall maze pos))
                        (map #(let [[dr dc] (directions %)]
                                [(+ (first pos) dr) (+ (second pos) dc)]))
                        (remove outside-maze)))]
  (core/dijkstra nodeset [0 0] [(dec height) (dec width)] (constantly 1) neighbours))

