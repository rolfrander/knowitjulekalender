(ns kalender2024.day10 
  (:require
    [rolfrander.puzzle-lib :as puzzle]
    [clojure.string :as str]))

(def joe (slurp "https://julekalender-backend.knowit.no/challenges/2024-10/files/joe.txt?disposition=inline"))

(def blanket (slurp "https://julekalender-backend.knowit.no/challenges/2024-10/files/teppe.txt?disposition=inline"))

(print blanket)

(defn parse-joe [input]
  (let [to-i (fn [x] (case x
                       \space 0
                       \x -2
                       (- (int x) (int \0))))]
    (->> (str/split-lines input)
         (mapv #(mapv to-i %)))))

(defn parse-blanket [input]
  (let [index (fn [x] (partition 2 (interleave (range) x)))]
    (for [[y l] (index (str/split-lines input))
          [x c] (index l)
          :when (not= c \space)]
      [x y (puzzle/char->digit c)])))

(defn calc-coze [joe blanket offset-x offset-y]
  (->> (map (fn [[x y c]]
              (let [joe-val (-> (get joe (+ offset-y y) [])
                                (get (+ offset-x x) 0))]
                (* joe-val c)))
            blanket)
       (reduce +)))

(defn find-max-cozy [j b] 
  (loop [x 0
         y 0
         mx 0]
    (cond
      (> x 10) (recur 0 (inc y) mx)
      (> y 10) mx
      :else (recur (inc x) y
                   (max mx (calc-coze j b x y))))))

(defn mirror-blanket [b]
  (map (fn [[x y c]] [(- 10 y) x c]) b))

(defn print-blanket [b]
  (let [b (into {} (map (fn [[x y c]] [[x y] c])) b)]
    (doseq [y (range 10)
            x (range 10)]
      (when (and (= x 0) (> y 0)) (println))
      (if (contains? b [x y])
        (print (b [x y]))
        (print \.)))))

(defn rot-blanket [b]
  (map (fn [[x y c]] [y x c]) b))

(defn solve [joe blanket]
  (let [j (parse-joe joe)
        b (parse-blanket blanket)]
    (->> (mapcat (juxt identity mirror-blanket) (take 4 (iterate rot-blanket b)))
         (map #(find-max-cozy j %))
         (reduce max))))

(solve joe blanket)
