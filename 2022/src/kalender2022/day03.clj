(ns kalender2022.day03 
  (:require [clojure.string :as string]
            [rolfrander.puzzle-lib :as puzzle-lib]))

(def gaver-text (slurp "https://julekalender-backend.knowit.no/challenges/2022-03/files/pakker.csv"))

(defn parse-gifts [input]
  (->> (string/split-lines input)
       (drop 1)
       (map #(string/split % #","))
       (map (partial mapv puzzle-lib/str->long))
       (map (partial zipmap [:w :h :l]))
       ))

(def gifts (parse-gifts gaver-text))

;(let [c (count gaver-text)]
;  (println (subs gaver-text (- c 100) c)))
;(clojure.pprint/pprint (drop 990 gifts))

(def test-gifts [{:w 30 :h 30 :l 20}
                 {:w 30 :h 30 :l 30}
                 {:w 25 :h 30 :l 20}])

(take 5 gifts)

(def permutations [[:w :h :l]
                   [:h :l :w]
                   [:l :w :h]
                   [:l :h :w]
                   [:h :w :l]
                   [:w :l :h]])

(defn paper-for-all-orientations [box]
  (let [calc-paper (fn [[a b c]]
                     [(* 2 (+ (a box) (b box)))
                      (+ (b box) (c box))])]
    (map calc-paper permutations)))

(defn calc-paper-from-roll [box]
  (let [all-orientations (paper-for-all-orientations box)
        wrap-across (filter (comp (partial >= 110) first) all-orientations)]
    (if (empty? wrap-across)
      (first (apply min-key first all-orientations))
      (second (apply min-key second wrap-across)))))

(paper-for-all-orientations (nth test-gifts 2))

(->> (map calc-paper-from-roll gifts)
     (reduce +))