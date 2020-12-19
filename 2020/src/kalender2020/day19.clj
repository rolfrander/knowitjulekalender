(ns kalender2020.day19
  (:require [clojure.string :as str]))

(set! *unchecked-math* false)
(set! *warn-on-reflection* true)

;; https://julekalender-backend.knowit.no/challenges/19/attachments/input.txt

(def testdata1 "1 3 [Jenny, Alvin, Greger, Petra, Olaug, Olaf]")
(def testdata2 "2 3 [Jenny, Alvin, Greger, Petra, Olaug, Olaf]")
(def testdata3 "3 3 [Jenny, Alvin, Greger, Petra, Olaug]")
(def testdata4 "4 3 [Jenny, Alvin, Greger, Petra, Olaug, Olaf]")

(def ^:dynamic *debug* false)

(defn parse [input]
  (->> input
       str/split-lines
       (map (fn [line]
              (let [[_line rule cnt names] (re-matches #"([1234]) ([0-9]+) \[([^\]]+)\]" line)]
                {:rule (Integer/parseInt rule)
                 :cnt (Integer/parseInt cnt)
                 :names (str/split names #", ")})))))

;(parse "1 3 [asdf, ewrt, æøå]")
;; => ({:rule 1, :cnt 3, :names ["Jenny" "Alvin" "Greger" "Petra" "Olaug" "Olaf"]})

; https://stackoverflow.com/questions/1394991/clojure-remove-item-from-vector-at-a-specified-location
(defn vec-remove
  "remove elem in coll"
  [pos coll]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

(defn vec-remove-2
  "remove 2 elem in coll"
  [pos coll]
  (vec (concat (subvec coll 0 pos) (subvec coll (+ pos 2)))))

(defn rotate-right [^long pos ^long cnt ^long len]
  (mod (+ pos cnt) len))

(defn rotate-left [^long pos ^long cnt ^long len]
  (mod (- pos cnt) len))

(defn debug-print [old oldpos newpos remove]
  (if *debug*
    (let [rotate (fn [l pos] (mapv (fn [i] (get l (mod (+ i pos) (count l)))) (range 0 (count l))))
          new (rotate old newpos)
          old (rotate old oldpos)]
      (printf "[%s] -> [%s] fjerne: %d\n" (str/join ", " old) (str/join ", " new) remove)
      (flush))))

(binding [*debug* true]
  (debug-print ["a" "b" "c" "d" "e" "f"] 2 3 4))

(defn exec-1 [cnt names]
  (loop [names names
         pos 0
         iter (count names)]
    (if (> iter 1)
      (let [newpos (rotate-left pos cnt iter)
            newlist (vec-remove newpos names)]
        (debug-print names pos newpos 0)
        (recur newlist
               newpos
               (dec iter)))
      names)))



(defn exec-2 [cnt names]
  (loop [names names
         pos 0
         remove-chair 0
         iter (count names)]
    (if (> iter 1)
      (let [remove-chair (if (>= remove-chair iter) 0 remove-chair)
            newpos (rotate-left pos cnt iter)
            remove-chair-shifted (rotate-right newpos remove-chair iter)
            newlist (vec-remove remove-chair-shifted names)]
        (debug-print names pos newpos remove-chair)
        (recur newlist
               (if (> newpos remove-chair-shifted) (dec newpos) newpos)
               (inc remove-chair)
               (dec iter)))
      names)))

;; hvilken skal fjernes?
;; 7 - 3
;; 6 - 2 3
;; 5 - 2
;; 4 - 1 2
;; 3 - 1
;; 2 - 0

(defn exec-3 [cnt names]
  (loop [names names
         pos 0
         iter (count names)]
    (if (> iter 1)
      (let [newpos (rotate-left pos cnt iter)
            remove-chair (rotate-right newpos (quot (dec iter) 2) iter)
            remove-chair-2 (rotate-right remove-chair 1 iter)
            remove-cnt (if (odd? iter) 1 2)
            newlist (cond (= iter 2)         (vec-remove newpos names)
                          (= remove-cnt 1)   (vec-remove   remove-chair names)
                          (> remove-chair-2 remove-chair) (vec-remove-2 remove-chair names)
                          :else (subvec names (inc remove-chair-2) remove-chair))]
        (if *debug* (print newpos remove-chair remove-chair-2 (first names) (first newlist)))
        (debug-print names pos newpos (quot (dec iter) 2))
        (recur newlist
               (- newpos 
                  (if (> newpos remove-chair) 1 0)
                  (if (and (= 2 remove-cnt) (> newpos remove-chair-2)) 1 0))
               (- iter remove-cnt)))
      names)))

(mod (+ 58 (quot (dec 63) 2)) 63)

(defn exec-4 [cnt names]
  (loop [names names
         pos 0
         iter (count names)]
    (if (> iter 1)
      (let [newpos (rotate-left pos cnt iter)
            remove-chair (rotate-left newpos 1 iter)
            newlist (vec-remove remove-chair names)]
        (debug-print names pos newpos (dec iter))
        (recur newlist
               (if (> newpos remove-chair) (dec newpos) newpos)
               (dec iter)))
      names)))

(defn exec [spec]
  (let [{:keys [rule cnt names]} spec]
    (first (case rule
             1 (exec-1 cnt names)
             2 (exec-2 cnt names)
             3 (exec-3 cnt names)
             4 (exec-4 cnt names)))))

(def data
  (->> "https://julekalender-backend.knowit.no/challenges/19/attachments/input.txt"
       slurp
       parse))

(defn rotate-elves [elves steps]
  (let [steps (mod (- steps) (count elves))]
    (vec (concat (subvec elves steps) (subvec elves 0 steps)))))

(defn play [spec]
  (let [{:keys [rule cnt names]} spec]
    (loop [names names
           i 0] ; i brukes bare i regel 2
      (if *debug* (println i names))
      (if (= (count names) 1)
        (first names)
        (let [names (rotate-elves names cnt)
              chaircount (count names)
              i (if (>= i chaircount) 0 i)]
          (case rule
            1 (recur (vec-remove 0 names) (inc i))
            2 (recur (vec-remove (mod i chaircount) names) (inc i))
            3 (recur (cond
                       (= 2 chaircount)  (vec-remove   0 names)
                       (odd? chaircount) (vec-remove   (quot (dec chaircount) 2) names)
                       :else             (vec-remove-2 (quot (dec chaircount) 2) names))
                     (inc i))
            4 (recur (vec-remove (dec chaircount) names) (inc i))))))))

(->> data
     (map exec)
     frequencies
     (apply max-key val)
     )



(let [diff (first (filter #(not= (exec %) (play %)) data))]
  (binding [*debug* true]
    (if diff
      [(doall (exec diff))
       (doall (play diff))]
      "all equal!")))

(apply max-key val {"a" 1, "b" 3, "c" 2})

(binding [*debug* true]
  (->> (str/join "\n" [testdata1 testdata2 testdata3 testdata4])
       parse
       (mapv play)))

(vec-remove 0 (rotate-elves (:names (first (parse testdata1))) 3))

(binding [*debug* true]
  [;(let [{:keys [_rule cnt names]} (first (parse testdata1))] (exec-1 cnt names))
   (let [{:keys [_rule cnt names]} (first (parse testdata2))] (exec-2 cnt names))
   ;(let [{:keys [_rule cnt names]} (first (parse testdata3))] (exec-3 cnt names))
   ;(let [{:keys [_rule cnt names]} (first (parse testdata4))] (exec-4 cnt names))
   ])