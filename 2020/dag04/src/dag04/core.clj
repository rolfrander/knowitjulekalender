(ns dag04.core
  (:gen-class))

(def testdata1 "HHOOVVNN" ) ;4

(def testdata2 "HHHHHHOOOOVVNNNVVOVVNN") ;14

(def testdata3 "HHHHHHOOOOOOVVVVVVNNNNHOHNHOHNNVVVVN") ;30

; Grunnleggende algoritme:
; vi starter i punkt 0,0 i et (tilnærmet) ubegrenset koordinatsystem
; så navigerer vi i henhold til instruksjonene, registrerer alle vertikale
; linjer og knytter dem til den øvre vertex,
; 
; det betyr at for N bruker vi startpunktet mens for O bruker vi sluttpunktet
; disse organiserer vi per rad (y-koordinat) og sorterer langs x-koordinat
; så summerer vi avstanden mellom hvert par, altså
; 
; sum( x-for-linje-(2n+1) - x-for-linje-(2n) )

(defrecord vertex [^int x
                   ^int y])

(def origo (vertex. 0 0))

(defn up    [^vertex v] (vertex. (:x v) (inc (:y v))))
(defn down  [^vertex v] (vertex. (:x v) (dec (:y v))))
(defn left  [^vertex v] (vertex. (dec (:x v)) (:y v)))
(defn right [^vertex v] (vertex. (inc (:x v)) (:y v)))

; finne alle vertikale linjer
(defn navigate-and-collect-vectors [input]
  (let [input-length (count input)]
    (loop [pos origo
           i 0
           return ()]
      (if (< i input-length)
        (case (nth input i)
          \O ; flytt først, register etterpå
          (let [newpos (up pos)]
            (recur newpos (inc i) (conj return newpos)))
          
          \N ; registrer pos før flytt
          (recur (down pos) (inc i) (conj return pos))
          
          \H (recur (right pos) (inc i) return)
          
          \V (recur (left pos) (inc i) return))
        return))))

; finne alle vertikale linjer
(defn navigate-and-collect-vectors [input]
  (let [input-length (count input)]
    (loop [x 0
           y 0
           i 0
           return ()]
      (if (< i input-length)
        (case (nth input i)
          \O ; flytt først, register etterpå
          (let [new-y (inc y)]
            (recur x new-y (inc i) (conj return (vertex. x new-y))))

          \N ; registrer pos før flytt
          (recur x (dec y) (inc i) (conj return (vertex. x y)))

          \H (recur (inc x) y (inc i) return)

          \V (recur (dec x) y (inc i) return))
        return))))

; for hver liste, velg ut x verdier og sorter
; v er en assosiativ struktur hvor hver verdi er en liste av vertex
(defn sort-x [v]
  (into {} (map (fn [[row lines]]
                  [row (sort (map :x lines))]))
        v))

; grupper x-verdier i par og akkumuler
(defn accumulate [v]
  (->> v
       vals
       (map #(partition 2 %))
       (map (fn [list-of-v-pairs]
              (->> list-of-v-pairs
                   (map (fn [[start end]] (- end start)))
                   (reduce +))))
       (reduce +)))
  
  
(defn compute-area [data]
  (->> data
       navigate-and-collect-vectors
       (group-by :y)
       sort-x
       accumulate))

(compute-area testdata1)
(compute-area testdata2)
(compute-area testdata3)

(compute-area (slurp "https://julekalender-backend.knowit.no/challenges/5/attachments/rute.txt"))


(map :x (list (vertex. 1 0) (vertex. 3 0) (vertex. 2 0) (vertex. 4 0)))