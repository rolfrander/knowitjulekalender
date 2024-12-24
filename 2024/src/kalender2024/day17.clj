(ns kalender2024.day17
  (:require
   [clojure.string :as str]
   [rolfrander.puzzle-lib :as puzzle]))

(def data (slurp "https://julekalender-backend.knowit.no/challenges/2024-17/files/tall.txt?disposition=inline"))

(def tall (map puzzle/str->long (str/split-lines data)))

(def max-tall (apply max tall))

(defn get-digits [^long n]
  (if (< n 10)
    [n]
    (conj (get-digits (quot n 10)) (mod n 10))))

(defn calc-pseudo-vampires [prev-factors next-factor-to-try]
  (let [partial-product (apply * prev-factors)]
    (concat (when (seq prev-factors)
              (for [b (range next-factor-to-try max-tall) :while (< (* b partial-product) max-tall)
                    :let [prod (* b partial-product)
                          factors (conj prev-factors b)
                          digits-factors (vec (sort (mapcat get-digits factors)))
                          digits-prod (vec (sort (get-digits prod)))]
                    :when (= digits-factors digits-prod)]
                [prod factors]))

            (mapcat  #(calc-pseudo-vampires (conj prev-factors %) %)
                     (range next-factor-to-try (Math/sqrt (/ max-tall partial-product)))))))

(time (def pseudo-vampires (doall (calc-pseudo-vampires [] 2))))

(def is-pseudo-vampire? (into {} pseudo-vampires))

(apply + (filter is-pseudo-vampire? tall))


