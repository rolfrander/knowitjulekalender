(ns kalender2024.day14 
  (:require
    [rolfrander.puzzle-lib :as puzzle]
    [clojure.string :as str]))

(def start-state (vec (take 10 (repeat 100000))))

(defn calc [start-state]
  (loop [current start-state
         siffer-seq []
         i 9
         line 0]
    (if (and (= (count siffer-seq) 10) (= 9 i))
      [line siffer-seq]
      (let [siffer (conj (puzzle/digits (max 0 (current i)))
                         i)
            [new-state nye-siffer-tomme]
            (reduce (fn [[state nye] siffer]
                      (let [v (dec (get state siffer))]
                        [(assoc state siffer v) (if (= v 0) (conj nye siffer) nye)]))
                    [current []]
                    siffer)]
        ;(when (seq nye-siffer-tomme) (println new-siffer-seq new-state))
        (recur new-state
               (reduce conj siffer-seq nye-siffer-tomme)
               (mod (dec i) 10)
               (if (= 0 i) (inc line) line))))))

(let [[lines order] (calc start-state)]
  (str lines " " (str/join "," order)))

