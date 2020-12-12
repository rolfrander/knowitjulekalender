(ns kalender2020.dag12
  (:require [clojure.string :as str]))

(def testdata "Alvor (Alv Alf Alvaro (Halfrid Halvar Halvard (Alvilde Alva (Alfie Alvor Joralv) Alfonse)) Calvin (Tjalve Alvbert Alvard))")



(let [m (re-matcher #"[a-zA-Z]+| +|\(|\)" (slurp "https://julekalender-backend.knowit.no/challenges/12/attachments/family.txt"))
      gencount (int-array 1000)]
  (letfn [(generation [^long gen]
            (loop []
              (when-let [token (re-find m)]
                (case token
                  " " (recur)
                  "(" (do (generation (inc gen))
                          (recur))
                  ")" () ; noop, return from recursion
                  (do (aset-int gencount gen
                                (inc (aget gencount gen)))
                      (recur))))))]
    (generation 0))
  (apply max gencount))
;; => 5965

(let [m (re-matcher #"[a-zA-Z]+| +|\(|\)" (slurp "https://julekalender-backend.knowit.no/challenges/12/attachments/family.txt"))
      gencount (int-array 1000)
      tokenlist (take-while seq (repeatedly (partial re-find m)))]
  (reduce (fn [generation token]
            (case token
              " " generation
              "(" (inc generation)
              ")" (dec generation)
              (do (aset-int gencount generation
                            (inc (aget gencount generation)))
                  generation)))
          0
          tokenlist)
  (apply max gencount))


(def x (partial re-find (re-matcher #"a" "aaababba")))

(x)