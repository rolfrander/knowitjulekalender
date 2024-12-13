(ns kalender2024.day13
  (:require
   [rolfrander.puzzle-lib :as puzzle]
   [clojure.string :as str]))

(def testdata "J/65/B/2-1/AB")
(def data (slurp "https://julekalender-backend.knowit.no/challenges/2024-13/files/salary.txt?disposition=inline"))

(defn parse-line [l]
  (let [[_linje lag spilletid hjemmeborte m-hjemme m-borte personligbonus] 
        (re-matches #"([A-Z]+)/(\d+)/([HB])/(\d+)-(\d+)/([ABS]*),?" l)
        hjemme (= hjemmeborte "H")]
    {:motstander   lag
     :spilletid    (puzzle/str->long spilletid)
     :hjemme?      hjemme
     :m-egne       (puzzle/str->long (if hjemme m-hjemme m-borte))
     :m-mots       (puzzle/str->long (if hjemme m-borte m-hjemme))
     :assists      (count (filter #{\A} personligbonus))
     :score        (count (filter #{\S} personligbonus))
     :banens-beste (boolean (some #{\B} personligbonus))}))

(def motstanderbonus {"FCB" 1
                      "J" 2
                      "NT" 1
                      "NF" 1
                      "RC" 3
                      "SP" 2
                      "VM" 3})

(defn resultat [egne andres]
  (condp apply [egne andres]
    < -1
    = 0
    > 1))

(defn kamp-bonus [kamp antall-tap-på-rad]
  (let [kampresultat (resultat (:m-egne kamp) (:m-mots kamp))
        vinn (> kampresultat 0)
        bonusprosent (+ (* 5 (- (:m-egne kamp) (:m-mots kamp)))
                        (:assists kamp)
                        (* 2 (:score kamp))
                        (if (:banens-beste kamp) 2 0)
                        (* (motstanderbonus (:motstander kamp)) kampresultat)
                        (if (and (> antall-tap-på-rad 0) vinn)
                          (+ 2 antall-tap-på-rad)
                          0))]
    (* (:spilletid kamp) (+ 100 bonusprosent))))

(->> (str/split-lines data)
     (reduce (fn [[total antall-tap-på-rad] l]
               (let [k (parse-line l)
                     akkumulert-utbetaling (+ total (kamp-bonus k antall-tap-på-rad))
                     taps-teller (case (resultat (:m-egne k) (:m-mots k))
                                   -1 (inc antall-tap-på-rad)
                                   0 antall-tap-på-rad
                                   1 0)]
                 [akkumulert-utbetaling taps-teller]))
             [0 0])
     first)