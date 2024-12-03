(ns kalender2024.day03
  (:require [clojure.string :as str]
            [rolfrander.puzzle-lib :as puzzle]))

(def menu ["ris"
           "erter"
           "gulrøtter"
           "reinsdyrkjøtt"
           "julekringle"])

(def fill-sequence {"ris" (cycle [0 0 1 0 0 2])
                    "erter" (cycle [0 3 0 0])
                    "gulrøtter" (concat (repeat 30 0) (cycle [0 1 0 0 0 8]))
                    "reinsdyrkjøtt" (repeat 0)
                    "julekringle" (repeat 0)
                    "rein" (concat [100 80 40 20 10] (repeat 0))
                    "rein-cnt" 50})

(def start-plate (zipmap menu (repeat 100)))

(defn eat [plate]
  (let [eat-2 (fn [food-1 food-2 plate]
                (-> plate
                    (update food-1 #(max 0 (- % 5)))
                    (update food-2 #(max 0 (- % 3)))))
        eat-1 (fn [food amount plate]
                (update plate food #(max 0 (- % amount))))
        check-2 (fn [food-1 food-2]
                  (and (> (plate food-1) 0)
                       (> (plate food-2) 0)))
        check-1 (fn [food amount]
                  (> (plate food) 0))]
    (cond (check-2 "ris" "erter")       (eat-2 "ris" "erter" plate)
          (check-2 "ris" "gulrøtter")   (eat-2 "ris" "gulrøtter" plate)
          (check-2 "erter" "gulrøtter") (eat-2 "erter" "gulrøtter" plate)
          (check-1 "ris" 5)             (eat-1 "ris" 5 plate)
          (check-1 "erter" 5)           (eat-1 "erter" 5 plate)
          (check-1 "gulrøtter" 5)       (eat-1 "gulrøtter" 5 plate)
          (check-1 "reinsdyrkjøtt" 2)   (eat-1 "reinsdyrkjøtt" 2 plate)
          (check-1 "julekringle" 1)     (eat-1 "julekringle" 1 plate)
          :else plate)))

(defn refill [plate fill]
  (let [[plate fill] (reduce (fn [[p f] food]
                               [(update p food #(+ % (first (f food))))
                                (update f food rest)])
                             [plate fill]
                             menu)]
    (if (= 0 (plate "reinsdyrkjøtt"))
      (if (= 0 (fill "rein-cnt"))
      ; tomt for rein og telleren er null
        [(update plate "reinsdyrkjøtt" #(+ % (first (fill "rein"))))
         (-> fill
             (update "rein" rest)
             (assoc "rein-cnt" 50))]
      ; tomt for rein, driver og teller ned
        [plate
         (update fill "rein-cnt" dec)])
      [plate fill])))

(refill (assoc start-plate "reinsdyrkjøtt" 0)
        (assoc fill-sequence "rein-cnt" 0))

(loop [i 0
       plate start-plate
       fill fill-sequence]
  (when (or  (<= i 73)
             (and (>= i 265) (<= i 270)))
    (println i plate (take 3 (fill "ris"))))
  (if (= 0 (plate "julekringle")) 
    i
    (let [plate (eat plate)
          [plate fill] (refill plate fill)]
      (recur (inc i) plate fill))))

(drop 22 (fill-sequence "ris"))

(eat {"ris" 0, "erter" 0, "gulrøtter" 155, "reinsdyrkjøtt" 350, "julekringle" 100})
