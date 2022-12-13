(ns kalender2022.day11 
  (:require [clojure.string :as string]))

(def chksum-vectors
  [[1,  3,  5,  7,  9, 11, 13, 15]
   [2,  3,  6,  7, 10, 11, 14, 15]
   [4,  5,  6,  7, 12, 13, 14, 15]
   [8,  9, 10, 11, 12, 13, 14, 15]])

(defn check-for-errors [bitmap]
  (let [check-one-vector (fn [bits]
                           (if (= 1 (apply bit-xor (map bitmap bits)))
                             (first bits)
                             0))]
    (apply + (map check-one-vector chksum-vectors))))

(def testdata-1
  [0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0,
   0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0])

(def data [0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0,
           0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0,
           0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0])

(defn partition-v [cnt vector]
  (map #(subvec vector (* cnt %) (* cnt (inc %)))
       (range 0 (/ (count vector) cnt))))

(defn bitflip [vector i]
  (assoc vector i (- 1 (vector i))))


(defn fix-data [data]
  (mapv (fn [vector]
          (let [error (check-for-errors vector)]
            (if (= error 0)
              vector
              (bitflip vector error))))
        (partition-v 16 data)))


(defn print-data [data]
  (let [remove-chksum (fn [d]
                        (map (partial get d) [3 5 6 7 9 10 11 12 13 14 15]))]
    (->> (map #(string/join (remove-chksum %)) data)
         string/join)
    ))

;(check-for-errors (first (partition-v 16 testdata-1)))

(print-data (fix-data data))
