(ns kalender2020.dag11
  (:require [clojure.string :as str]))

(defn parse [input]
  (->> input
       str/split-lines
       (map vec)
       ))

(def a (int \a))

(defn char-rot [character distance]
  (let [letter (- (int character) a)]
    (char (+ a (mod (+ letter distance) 26)))))

(defn hint-mangle
  [word]
  (->> (rest word)
       (map #(char-rot % 1))
       (mapv #(char-rot %1 (- (int %2) a)) word)))

(defn transpose [hints]
  (map (fn [variant]
         (str/join (mapv #(get % variant) hints)))
       (range (count (first hints)))))

(defn generate-strings-from-hint [hint]
  (->>
   hint
   (iterate hint-mangle)
   (take (count hint))
   transpose))

(defn find-passwordhint [hints pass-re]
  (some #(re-find pass-re %) hints))

(defn search-hints [input password]
  (let [pass-re (re-pattern password)]
    (->> input
         (parse)
         (remove #(-> %
                     generate-strings-from-hint
                     (find-passwordhint pass-re)
                     not))
         (map str/join))))

(def testdata "juletre")
(def testpass "gxno")

(def data (slurp "https://julekalender-backend.knowit.no/challenges/11/attachments/hint.txt"))

(time (search-hints data "eamqia"))

