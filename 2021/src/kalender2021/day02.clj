(ns advent2021.day02
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure-csv.core :as csv]))

;; point format: LON LAT in degrees
(def testdata "city,location
Victorville,Point(-117.288333333 34.536111111)
Florencio Varela,Point(-58.383333333 -34.816666666)
")

(def earth-radius 6371)

(defn deg-to-rad [deg]
  (* deg (/ Math/PI 180)))

(def north-pole
  [(deg-to-rad 0)
   (deg-to-rad 90)])

(defn parse-point [well-known-text-point]
  ;https://stackoverflow.com/questions/365826/calculate-distance-between-2-gps-coordinates
  (if (.startsWith well-known-text-point "Point(")
    (map #(deg-to-rad (Double/parseDouble %)) (string/split (.substring well-known-text-point 6) #"[() ]"))
    (throw (RuntimeException. (str "not a well known text point: " well-known-text-point)))))

(defn sq [d] (* d d))

(defn distance [[lon1 lat1] [lon2 lat2]]
  (let [dlat (- lat2 lat1)
        dlon (- lon2 lon1)
        a (+ (sq (Math/sin (/ dlat 2)))
             (* (sq (Math/sin (/ dlon 2)))
                (Math/cos lat1)
                (Math/cos lat2)))
        c (* 2 (Math/atan2 (Math/sqrt a)
                           (Math/sqrt (- 1 a))))]
    [a c
     (* earth-radius c)]))


;(distance [(deg-to-rad 0) (deg-to-rad 51.5)] 
;          [(deg-to-rad -77.1) (deg-to-rad 38.8)])
; fasit: 5918.185064

;(parse-point "Point(-117.288333333 34.536111111)")

;(def parse-wkt [input]
(let [input testdata]
  (->> input
       csv/parse-csv
       rest
       (map (fn [[city point]] [city (parse-point point)]))
       ))

