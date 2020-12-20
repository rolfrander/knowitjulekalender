(ns kalender2020.day21
  (:require [clojure.string :as str]))

(set! *unchecked-math* false)
(set! *warn-on-reflection* true)

(def testdata "Goodlight,3
---
Mittenmilk,2
---
Sparklefrost,2
Claus,1
Chimneyjoy,1
Plumcarol,5
Milkwine,2")

(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
    (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

(defmethod print-method clojure.lang.PersistentQueue [q, w] ; Overload the printer for queues so they look like fish
  (print-method '<- w)
  (print-method (seq q) w)
  (print-method '-< w))

(defn data-from-string [input]
  (str/split-lines input))

(defn parse [input-list]
  (map #(if (= "---" %)
          :available
          (str/split % #",")) input-list))

(defn first-nonempty-index [s]
  (loop [[f & s] s
         i 0]
    (if (or (nil? f) (empty? f))
      (if (empty? s)
        nil
        (recur s (inc i)))
      i)))

(defn treat [queues]
  (let [priority (first-nonempty-index queues)]
    (if (nil? priority)
      ["" queues]
      [(peek (get queues priority))
       (update queues priority pop)])))

(defn wait [queues patient]
  (let [[name priority] patient]
    (update queues (Integer/parseInt priority) #(conj % name)))
  )
  
(defn er [data me]
  (let [[treated remaining]
        (loop [[patient & others] data
               q (vec (take 6 (repeatedly queue)))
               i 0]
          (if (nil? patient)
            [i q]
            (if (= :available patient)
              (let [[treated-patient remaining] (treat q)]
                (if (= treated-patient me)
                  [i nil] ;; return
                  (recur others remaining (inc i))))
              (recur others (wait q patient) i))))]
    (loop [i treated
           q remaining]
      (let [[treated-patient remaining] (treat q)]
        (if (= treated-patient me)
          i
          (recur (inc i) remaining))))))

(let [data (parse (data-from-string testdata))
      me "Claus"]
  (er data me))

(with-open [in (clojure.java.io/reader "dag21.txt")]
   (er (parse (line-seq in)) "Claus")))
