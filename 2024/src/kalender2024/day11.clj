(ns kalender2024.day11
  (:require
   [rolfrander.puzzle-lib :as puzzle]
   [clojure.string :as str]))

(def grep (slurp "https://julekalender-backend.knowit.no/challenges/2024-11/files/grep.txt?disposition=inline"))

(def arm-v "        xxx
      xxxxxxx
     xxxxxxxxx
    xxxxxxxxxxx
   xxxxxxxxxxxx
  xxxxxxxxxxxxx
  xxxxxxxxxxxxx
 xxxxxxxxxxxxx
 xxxxxxxxxxxx
          o")

(def arm-h "      xxx
    xxxxxxx
   xxxxxxxxx
  xxxxxxxxxxx
  xxxxxxxxxxxx
  xxxxxxxxxxxxx
  xxxxxxxxxxxxx
   xxxxxxxxxxxxx
    xxxxxxxxxxxx
      o")

(defn parse-arm [in]
  (let [a (puzzle/parse-map in :space \space)
        arm-reach (get-in a [:markings \x])
        height (:height a)
        [o-x o-y] (first (get-in a [:markings \o]))]
    (map (fn [[x y]] [(- o-y y) (- x o-x)]) arm-reach)))

(defn parse-grep [in]
  (->> (re-seq #"\d+" in)
       (map puzzle/str->long)
       (partition 2)
       (into #{})))

(defn neste-flytt [pos arm grep]
  (->> (map (partial map + pos) arm)
       (filter grep)))

(let [v (parse-arm arm-v)
      h (parse-arm arm-h)
      grep (parse-grep grep)
      start [[0 250] [0 250] :v]]
  ;(neste-flytt [20 240] v grep)
 ; (filter grep (for [x (range 240 250) y (range 19 30)] [y x]))
  ;(->> (map (partial map + [20 240]) v)
  ;     (filter grep))
  (apply max-key second grep))

(defn finn-rute [arm-a arm-b grep pos]
  (if (= pos [999 749])
    (list pos)
    (let [neste (neste-flytt pos arm-a grep)]
      (if (empty? neste)
        nil
        (cons pos (keep (partial finn-rute arm-b arm-a grep) neste))))))

(let [v (parse-arm arm-v)
      h (parse-arm arm-h)
      grep (parse-grep grep)
      posisjoner (mapcat #(list [% :v] [% :h]) grep)
      start [[0 250] :v]
      sq (fn [x] (* x x))]
  (puzzle/dijkstra posisjoner
                   start
                   (fn [[pos neste-arm]]
                     ;(println pos neste-arm)
                     (if (= neste-arm :v)
                       (map #(vector % :h) (neste-flytt pos v grep))
                       (map #(vector % :v) (neste-flytt pos h grep))))
                   :weight-fn (fn [[[y1 x1] arm1]
                                   [[y2 x2] arm2]]
                                (if (= arm1 arm2) 999999
                                    (Math/sqrt (+ (sq (- y2 y1))
                                                  (sq (- x2 x1))))))

                   :dest? (fn [[pos _arm1]] (= pos [999 749]))
                   ;:dest? (fn [[[y x] _arm1]] (> y 100))
                   :result-type :dist
                   ))


(finn-rute (parse-arm arm-v)
           (parse-arm arm-h)
           (into #{} (filter (fn [[y x]] (< y 30)) (parse-grep grep)))
           [0 250])

