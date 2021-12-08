(ns kalender2021.day06
  (:require [clojure.string :as string]))


(def testdata "0,6
0,1
4,3
2,4")

(defn parse [data]
  (map (fn [line]
         (map #(Long/parseLong %) (string/split line #",")))
       (string/split-lines data)))

(parse testdata)

;; datastructure:
;;  for a stack of stable packages: for each position, what is the current height?
;;  position n is the position between (n-1) and n


(defn stable-height [stack x len]
  ;; for a given stack (defined as above) and a package starting on x with length len
  ;; if it is stable, return the height of the stack where it is placed, if not, return nil
  (let [halflen (+ (quot len 2) (rem len 2))
        max-left (apply max (subvec stack x (+ x halflen)))
        max-right (apply max (subvec stack (- (+ x len) halflen) (+ x len)))]
    (if (= max-left max-right)
      max-left
      nil)))

(defn place-package [[stack counter] [x len]]
  (if-let [height (stable-height stack x len)]
    (loop [new-stack []
           i 0]
      (if (= i (.length stack))
        [new-stack counter]
        (if (and (>= i x)
                 (< i (+ x len)))
          (recur (conj new-stack (inc height)) (inc i))
          (recur (conj new-stack (nth stack i)) (inc i)))))
    [stack (inc counter)]))


(reduce place-package
       [(vec (repeat 20 0)) 0]
       (parse (slurp "https://julekalender-backend.knowit.no/rails/active_storage/blobs/redirect/eyJfcmFpbHMiOnsibWVzc2FnZSI6IkJBaHBBallDIiwiZXhwIjpudWxsLCJwdXIiOiJibG9iX2lkIn19--20b29549a475416a15aa81ff11b00da4c4103e67/pakker.txt?disposition=inline")))