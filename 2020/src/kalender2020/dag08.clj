(ns kalender2020.dag08
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def testdata "Nordpolen: (3, 4)
Rovaniemi: (1, 1)
Rovaniemi
Nordpolen")

(def data (slurp "https://julekalender-backend.knowit.no/challenges/8/attachments/input.txt"))

(defrecord point [x
                  y
                  id])


(defn distance ^long [a b]
  (let [xa (int (:x a))
        xb (int (:x b))
        ya (int (:y a))
        yb (int (:y b))]
    (+ (int (Math/abs (int (- xa xb))))
       (int (Math/abs (int (- ya yb)))))))

(defn route [a b]
  (let [count-squares (fn [a b]
                        (let [sign (Long/signum (- b a))]
                          (range (+ a sign) (+ b sign) sign)))]
    (as-> [] return
      (reduce #(conj %1 (point. %2 (:y a) -1))
              return
              (count-squares (:x a) (:x b)))
      (reduce #(conj %1 (point. (:x b) %2 -1))
              return
              (count-squares (:y a) (:y b))))))

(def origo (point. 0 0 -1))

(defn parse-input
  "returnerer to datasett: en mapping fra navn til koordinat som point og en liste med punkter som reiserute"
  [data]
  (->> data
       str/split-lines
       (reduce (fn [[places route counter] linje]
                 (let [pointmatch (re-matches #"([^:]*): [(]([0-9]+), ([0-9]+)[)].*" linje)]
                   (if pointmatch
                     (let [[_ name x y] pointmatch]
                       [(assoc places name (point. (Integer/parseInt x)
                                                   (Integer/parseInt y)
                                                   counter))
                        route
                        (inc counter)])
                     ; her glemmer vi stedteller, det er ok hvis vi antar at alle steder defineres 
                     ; først og reiserute etterpå
                     [places (conj route (get places linje))])))
               [{} [origo] 0])))

(def timelag-lookup ^doubles (double-array 100))

(dotimes [i (count timelag-lookup)]
  (aset-double timelag-lookup i
               (double (cond
                         (<= 50 i) 1.0
                         (<= 20 i) 0.75
                         (<= 5 i) 0.5
                         (<= 1 i) 0.25
                         :else 0.0))))

(defn get-timelag ^double [^long i]
  (if (>= i (count timelag-lookup))
    1.0
    (double (aget (doubles timelag-lookup) (int i)))))

(defn calculate-times [data]
  (let [[places rt] (parse-input data)
        local-clock (int-array (count places) 0.0)
        advance-time (fn [current-point]
                       (doseq [[_ point] places]
                         (let [dist    (int (distance current-point point))
                               clockid (int (:id point))
                               tick    (int (cond
                                              (<= 50 dist) 4
                                              (<= 20 dist) 3
                                              (<= 5 dist) 2
                                              (<= 1 dist) 1
                                              :else 0))]
                           (aset-int local-clock clockid
                                     (int (+ (aget local-clock clockid)
                                             tick))))))]
    (reduce (fn [current next]
              (doseq [step (route current next)]
                (advance-time step))
              next)
            rt)
    local-clock))

(defn timediff [data]
  (let [times (calculate-times data)
        [max min] (reduce (fn [[mx mn] next]
                            [(max mx next) (min mn next)])
                          [0 10000000]
                          times)]
    (/ (- max min) 4.0)))

(decompile (fn [data]
             (let [[places rt] (parse-input data)]
               (doall rt)
               (time (reduce (fn [current next]
                               (doseq [step (route current next)]
                                 (doseq [point (second places)]
                                   (distance point step)))
                               next)
                             rt)))))

(time
 (timediff data))


