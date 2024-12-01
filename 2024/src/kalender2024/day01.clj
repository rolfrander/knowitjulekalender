(ns kalender2024.day01
  (:require [clojure.string :as string]
            [rolfrander.puzzle-lib :as puzzle-lib]))

(def joe (slurp "https://julekalender-backend.knowit.no/challenges/2024-01/files/joe.txt?disposition=inline"))

(def blanket " xxx
xxxxx
xxxxx
xxxxx
xxxxx
  x
xxxxx
xxxxx
xxxxx
xxxxx
 xxx")

(defn parse-joe [input]
  (let [to-i (fn [x] (if (= x \space)
                       0
                       (- (int x) (int \0))))]
    (->> (string/split-lines input)
         (mapv #(mapv to-i %)))))

(defn parse-blanket [input]
  (let [index (fn [x] (partition 2 (interleave (range) x)))]
    (for [[y l] (index (string/split-lines input))
          [x c] (index l)
          :when (= c \x)]
      [x y])))

(defn calc-coze [joe blanket offset-x offset-y]
  (->> (map (fn [[x y]]
              (-> (get joe (+ offset-y y) [])
                  (get (+ offset-x x) 0)))
            blanket)
       (reduce +)))

(defn solve [joe blanket]
  (let [j (parse-joe joe)
        b (parse-blanket blanket)]
    (loop [x 0
           y 0
           mx 0]
      (cond
        (> x 7) (recur 0 (inc y) mx)
        (> y 29) mx
        :else (recur (inc x) y
                     (max mx (calc-coze j b x y)))))))

(solve joe blanket)
