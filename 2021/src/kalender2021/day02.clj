(ns advent2021.day02
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure-csv.core :as csv]))

;; point format: LON LAT in degrees
(def testdata "city,location
Victorville,Point(-117.288333333 34.536111111)
Florencio Varela,Point(-58.383333333 -34.816666666)
London,Point(0 51.5)
New York,Point(-77.1 38.8)
")

(def data (slurp "https://julekalender-backend.knowit.no/rails/active_storage/blobs/redirect/eyJfcmFpbHMiOnsibWVzc2FnZSI6IkJBaHBOUT09IiwiZXhwIjpudWxsLCJwdXIiOiJibG9iX2lkIn19--7bb23c39ab7eb5b367e3b0841b86e0667756397f/cities.csv"))

(def earth-radius 6371)

(defn deg-to-rad [deg]
  (* deg (/ Math/PI 180)))

(def north-pole
  [(deg-to-rad 0)
   (deg-to-rad 90)
   0])

(defn parse-point [well-known-text-point]
  ;https://stackoverflow.com/questions/365826/calculate-distance-between-2-gps-coordinates
  (if (.startsWith well-known-text-point "Point(")
    (map #(deg-to-rad (Double/parseDouble %)) (string/split (.substring well-known-text-point 6) #"[() ]"))
    (throw (RuntimeException. (str "not a well known text point: " well-known-text-point)))))

(defn add-cos-lat [points]
  (map (fn [[city [lon lat]]]
         [city [lon lat (Math/cos lat)]])
       points))

(defn sq [d] (* d d))

(defn distance [[lon1 lat1 coslat1] [lon2 lat2 coslat2]]
  (let [dlat (- lat2 lat1)
        dlon (- lon2 lon1)
        a (+ (sq (Math/sin (/ dlat 2)))
             (* (sq (Math/sin (/ dlon 2)))
                coslat1
                coslat2))
        c (* 2 (Math/atan2 (Math/sqrt a)
                           (Math/sqrt (- 1 a))))]
    (* earth-radius c)))


(distance [(deg-to-rad 0) (deg-to-rad 51.5) 0] 
          [(deg-to-rad -77.1) (deg-to-rad 38.8) 0])
; fasit: 5918.185064

;(parse-point "Point(-117.288333333 34.536111111)")

(defn parse-wkt [input]
  (->> input
       csv/parse-csv
       rest
       (map (fn [[city point]] [city (parse-point point)]))
       add-cos-lat
       (into (hash-map))))

(defn find-closest [current-pos cities]
  (reduce-kv (fn [res name pos]
            (let [d (distance current-pos pos)]
              (if (< d (second res))
                [name d]
                res)))
          ["" 40000] cities))

(find-closest north-pole (parse-wkt testdata))

(clojure.pprint/pprint
 (let [d (parse-wkt testdata)]
   (for [[name pos] d]
     [name (distance north-pole pos)])))

(let [[d final-pos] (loop [cities (parse-wkt data)
               curpos north-pole
               distance 0]
          (if (= 0 (count cities))
            [distance curpos]
            (let [[next-city leg] (find-closest curpos cities)]
              ;(println curpos next-city (get cities next-city))
              (recur (dissoc cities next-city)
                     (get cities next-city)
                     (+ distance leg)))))]
  (+ d (distance final-pos north-pole)))
