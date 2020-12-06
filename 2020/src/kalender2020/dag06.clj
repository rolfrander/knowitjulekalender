(ns kalender2020.dag06)

(set! *warn-on-reflection* true)

(def testdata "14,10,14,15,14,13,13,13,15,11")

(def testdata2 "10,14,14,13,13,13,15,14,11,15,11")

(defn read-value [data]
  (->> (clojure.string/split data #",")
       (map #(Integer/parseInt %))
       (int-array)))

(def data (slurp "https://julekalender-backend.knowit.no/challenges/6/attachments/godteri.txt"))

(defn best-packaging [elf-count value-count values curr-best-count curr-best-value]
  (let [elf-count (int elf-count)
        values (int-array values)
        [i weight value] (reduce (fn [[i lowest-weight highest-value] value]
                                   (cond
                                     (not= 0 (mod value elf-count)) [(inc i) lowest-weight highest-value]
                                     (or (> value highest-value)
                                         (and (< i lowest-weight)
                                              (= value highest-value))) [(inc i) i value]
                                     :else [(inc i) lowest-weight highest-value]))
                                 [0 curr-best-count curr-best-value]
                                 (take value-count values))]
    [weight value]))

(defn elf-candy-1 [^ints values elf-count]
  (let [iterations (inc (count values))]
    (loop  [curr-values (int-array iterations 0)
            next-values (int-array iterations 0)
            curr-best [iterations 0]
            line-no 1]
       ; inner loop, calculate line n+1 based on line n
      (if (< line-no iterations)
        (let [value-of-current-item (int (aget values (unchecked-dec line-no)))]
          (dotimes [i line-no]
            (let [current-weight (unchecked-inc i)
                  best-at-this-position (max (aget curr-values (int current-weight))
                                             (+ (aget curr-values (int i))
                                                value-of-current-item))]
              (aset-int next-values current-weight best-at-this-position)))
          
          (when (= 0 (mod line-no 1000))
            (println (/ line-no 100) "%" curr-best)
            (flush))
          (let [best-in-line (best-packaging elf-count (inc line-no) next-values (first curr-best) (second curr-best))]
            ;(println (clojure.string/join "," best-in-line) "," (clojure.string/join "," (seq next-values)))
            (recur next-values
                   curr-values
                   best-in-line
                   (inc line-no))))
        (let [[packs pieces] curr-best]
          (str (/ pieces elf-count) "," packs))))))

(defn elf-candy-2 [^ints values elf-count]
  (let [iterations (inc (count values))]
    (loop  [curr-values (int-array iterations 0)
            next-values (int-array iterations 0)
            line-no 1]
       ; inner loop, calculate line n+1 based on line n
      (if (< line-no iterations)
        (let [value-of-current-item (int (aget values (unchecked-dec line-no)))]
          (dotimes [i line-no]
            (let [current-weight (unchecked-inc i)
                  best-at-this-position (max (aget curr-values (int current-weight))
                                             (+ (aget curr-values (int i))
                                                value-of-current-item))]
              (aset-int next-values current-weight best-at-this-position)))

          (when (= 0 (mod line-no 1000))
            (println (/ line-no 100) "%")
            (flush))
          ;(println (clojure.string/join "," (seq next-values)))
          (recur next-values
                 curr-values
                 (inc line-no)))
        (let [[packs pieces] (best-packaging elf-count (inc line-no) curr-values iterations 0)]
          (str (/ pieces elf-count) "," packs))))))

(defn elf-candy-3 [^ints values elf-count]
  (loop [total (reduce + values)
         i (dec (count values))]
    (if (= 0 (mod total elf-count)) 
      (/ total elf-count)
      (recur (- total (aget values (int i)))
             (dec i)))))

(def values (read-value data))

(best-packaging 9 9 [0 54 27 81 72 54 65] 4 81)

(let [data (read-value testdata2)]
  (println (clojure.string/join "," (seq data)))
  (elf-candy-3 data 9))

(time (elf-candy-3 values 127))



(counted? (take 5 (iterate #(do (println %) (inc %)) 0)))

; 982,9977