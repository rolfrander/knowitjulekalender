(ns kalender2024.day21
    (:require [clojure.string :as str]
              [rolfrander.puzzle-lib :as puzzle]
              [clojure.data.json :as json]
              [clojure.java.io :as io]))

(def butikker
  (with-open [r (io/reader "src/kalender2024/butikker.csv")]
    (let [lines (rest (line-seq r))]
      (->> (map (fn [l] (let [[lat lon ris] (re-seq #"[0-9.-]+" l)]
                          {:lat (Double/parseDouble lat)
                           :lon (Double/parseDouble lon)
                           :ris (Long/parseLong ris)})) lines)
           (filter #(> (:ris %) 0))
           (filter #(< -10 (:lon %) 35))
           (filter #(< 55 (:lat %) 83))
           
           doall))))

;(count butikker)

(def norge
  (with-open [reader (io/reader "src/kalender2024/norge.geojson")]
    (->> (get-in (json/read reader :key-fn keyword) [:features 0 :geometry :coordinates])
         (map first)
         (mapv (fn [poly] 
                 (map #(let [[lon lat] %] {:lat lat :lon lon}) poly)))
         )))

;(first norge)

(defn inside-polygon?
  "determine if the point is inside the polygon using a ray-casting algoritm: 
   counting the number of edges in the polygon crossing the line to the north pole"
  [point polygon]
  (let [point-lon (:lon point)
        point-lat (:lat point)
        edges (partition 2 1 polygon) ; pair points to edges

        edges-crossing-lon ; return edges with one point on each side of the lon
        (filter (fn [[a b]] (or (<= (:lon a) point-lon (:lon b))
                                (<= (:lon b) point-lon (:lon a))))
                edges)

        ; formulas for euclidian geometry, not sure if it works on a sphere...
        ; https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
        ; x1 = (:lon a), y1 = (:lat a)
        ; x2 = (:lon b), y2 = (:lat b)
        ; x3 = point-lon,y3 = point-lat
        ; x4 = point-lon,y4 = 0
        ; => x3-x4 = 0, y3-y4 = y3
        edges-north-of-point
        (filter (fn [[a b]] (let [lat-crossing (/ (+ (- (* (:lon a) (:lat b))
                                                        (* (:lat a) (:lon b)))
                                                     (* (- (:lat a) (:lat b)) point-lon))
                                                  (- (:lon a) (:lon b)))]
                              (>= lat-crossing point-lat)))
                edges-crossing-lon)]
    (odd? (count edges-north-of-point))))

; https://stackoverflow.com/questions/27928/calculate-distance-between-two-latitude-longitude-points-haversine-formula
; https://en.wikipedia.org/wiki/Haversine_formula

(defn haversine-distance [a b]
  (let [r 6371
        p (/ Math/PI 180)
        hav (fn [theta] (/ (- 1 (Math/cos theta)) 2))
        d-lat (* (- (:lat b) (:lat a)) p)
        d-lon (* (- (:lon b) (:lon a)) p)
        lat-1 (* (:lat a) p)
        lat-2 (* (:lat b) p)

        hav-th (+ (hav d-lat) (* (Math/cos lat-1) (Math/cos lat-2) (hav d-lon)))
        d (* 2 r (Math/asin (Math/sqrt hav-th)))
        ]
    d))

;(haversine-distance {:lat 61.91 :lon 9.85}
;                {:lat 59.95 :lon 10})

; could possibly be done more efficiently with some crude pre-processing, but this only
; needs to be done once...
(def butikker-i-norge (filter #(some (partial inside-polygon? %) norge) butikker))

;(count butikker-i-norge)

(def nordpolen {:lat 90 :lon -0})

(group-by #(Math/signum (:lon %)) butikker-i-norge)

; (count butikker-i-norge)
;;=> 10

(for [l (take-while (complement empty?)
                        (iterate rest (conj butikker-i-norge nordpolen)))
      b (rest l)
      :let [a (first l)]
      ]
  [a b (haversine-distance a b)])

(java.util.Locale/setDefault java.util.Locale/US)
(defn format-coord [coord] 
  (let [fl-3 (fn [num] (/ (int (* num 1000)) 1000.0))]
    (format "(%3.3f,%3.3f)" (fl-3 (:lat coord)) (fl-3 (:lon coord)))))

(println (str/join "," (map format-coord butikker-i-norge)))

(def ordered-points (into [nordpolen] butikker-i-norge))

(def dist (mapv #(mapv (partial haversine-distance %) ordered-points)
                ordered-points))

(map (partial apply +) dist)

(->> (sort-by :lat ordered-points)
     (map format-coord))

(def order (let [nordpolen 0
                 jan-mayen 10
                 others (vec (range 1 10))
                 calc-dist (fn [v] (apply + (map (fn [[a b]] (get-in dist [a b]))
                                                 (partition 2 1 (concat [nordpolen jan-mayen] v [nordpolen])))))]
             (->> (map (juxt identity calc-dist) (puzzle/permute others))
                  (apply min-key second)
                  first)))

(println (str/join "," (map #(format-coord (ordered-points %))
                            (concat [0] order [0]))))

