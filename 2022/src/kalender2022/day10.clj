(ns kalender2022.day10
  (:require [clj-java-decompiler.core :as d]))

(set! *unchecked-math* :warn-on-boxed)

(def ^:const r_max 1000000.0)
(def ^:const 𝛼 0.2)
(def ^:const 𝛾 7.5E-05)
(def ^:const λ 83.0)
(def ^:const 𝛽 0.1)

(def ^:const R_0 125000.0)
(def ^:const U_0 3500.0)

(defn next-R [^double R ^double U]
  (Math/floor (+ R (/ (* 𝛼 R (- r_max R)) r_max)
                 (- (* 𝛾 U R)))))

(defn next-U [^double R ^double U]
  (Math/floor (+ U (/ (* 𝛾 U R) λ)
                 (- (* 𝛽 U)))))

(let [R_U (fn [[R U]] [(next-R R U) (next-U R U)])]
  (take 11 (iterate R_U [R_0 U_0])))

(defn after-n [R U n]
  (loop [i n
         R R_0
         U U_0]
    (if (= i 0)
      [R U]
      (recur (dec i)
             (next-R R U)
             (next-U R U)
             ))))

;(after-n R_0 U_0 10)

(time
 (let [stop 1000
       reinsdyr (long-array r_max -1)
       ulver (long-array 4000 -1)]
   (loop [i 0
          R R_0
          U U_0
          ]
     (if (= i stop)
       [i R U]
       (let [tidl-reinsdyr (aget reinsdyr (long R))
             tidl-ulv      (aget ulver    (long U))]
         (if (and (= tidl-reinsdyr tidl-ulv)
                  (not= tidl-reinsdyr -1))
           [i R U tidl-reinsdyr tidl-ulv]
           (do
             (aset-long reinsdyr (long R) i)
             (aset-long ulver (long U) i)
             (recur (inc i)
                    (next-R R U)
                    (next-U R U)))))))))

(time
 (let [stop 1000000]
   (loop [i 0
          R R_0
          U U_0
          tidl {}]
     (if (= i stop)
       [i R U]
       (if (tidl [R U])
         [i R U (tidl [R U])]
         (recur (inc i)
                (next-R R U)
                (next-U R U)
                (assoc tidl [R U] i)))))))

(time
 (let [stop 2000000000]
   (loop [i 0
          R R_0
          U U_0
          min-R R_0
          min-U U_0
          max-U 0]
     (if (= i stop)
       [i R U min-R min-U max-U]
       (recur (inc i)
              (next-R R U)
              (next-U R U)
              (min min-R R)
              (min min-U U)
              (max max-U U))))))

(* (- r_max 58120) (- 3555 1489))

(let [start 2517
      periode (- 2650 start)
      iterasjoner (quot (- (long 1E12) start) periode)]
  (- (long 1E12) (+ start (* periode iterasjoner))))

(after-n 111234.0 2366.0 (+ 74 2650))

