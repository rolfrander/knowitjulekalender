(ns kalender2020.day18
  (:require [clojure.string :as str]))

(def testdata ["is" "gnisning" "kauka" "baluba" "tarotkorta" "regninger" "ergre" "foobar" "tillit"])

(defn is-palindrom [word]
  (= word (reverse word)))

(comment defn is-palinstedrom [word]
         (let [[letter & wordtail] word
               pairs (reduce (fn [[pairlist cur] next]
                               (conj pairlist [cur next]))
                             [[] letter]
                             wordtail)
               halfway (apply max (map #(quot (count %) 2) [pairs word]))]
           (loop [p-cnt 0
                  w-cnt 0]
             ())))

(defn is-palinstedrom [^String word]
  (let [compare-pair (fn [l r]
                       (and (> (- r l) 2)
                            (= (.charAt word l) (.charAt word (dec r)))
                            (= (.charAt word (inc l)) (.charAt word r))))
        c (.length word)]
    (if (<= c 2) 
      false
      (loop [l 0
             r (dec c)
             palindrom true]
        (cond
          (>= l r) (if palindrom 'palindrom 'palinstedrom)
          (= (.charAt word l) (.charAt word r)) (recur (inc l) (dec r) palindrom)
          (= l (dec r)) 'palinstedrom
          (compare-pair l r) (recur (+ l 2) (- r 2) false)
          :else false)))))

(time 
 (with-open [in (clojure.java.io/reader "dag18-wordlist.txt")]
   (reduce (fn [count word]
             (if (= (is-palinstedrom word) 'palinstedrom)
               (inc count)
               count))
           0
           (line-seq in))))
;; => 252


(map #(vector % (is-palinstedrom %)) testdata)
;; => (["is" false]
;;     ["gnisning" palinstedrom]
;;     ["kauka" palinstedrom]
;;     ["baluba" palinstedrom]
;;     ["tarotkorta" palinstedrom]
;;     ["regninger" palindrom]
;;     ["ergre" palindrom]
;;     ["foobar" false]
;;     ["tillit" palindrom])

