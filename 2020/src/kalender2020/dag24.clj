(ns kalender2020.day24
  (:require [clojure.string :as str]))

(set! *unchecked-math* false)
(set! *warn-on-reflection* true)

(def ^:dynamic *debug* false)

(def testdata "11001100100010000000010000000000010000000010000001")

(defn exec [input]
  (loop [[h & houses] (re-seq #"[01]" input)
         i 0
         cap 10]
    (if (or (nil? h) (= cap 0))
      i
      (recur houses
             (inc i)
             (case h 
               "0" (dec cap)
               "1" (inc cap))))))

(exec testdata)
;; => 24

(exec (slurp "https://julekalender-backend.knowit.no/challenges/24/attachments/rute.txt"))
;; => 3333490

