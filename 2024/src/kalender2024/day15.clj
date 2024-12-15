(ns kalender2024.day15 
  (:require
    [clojure.string :as str]
    [rolfrander.puzzle-lib :as puzzle]))

(def testdata (slurp "https://julekalender-backend.knowit.no/challenges/2024-15/files/eksempler.txt?disposition=inline"))
(def data (slurp "https://julekalender-backend.knowit.no/challenges/2024-15/files/transaksjoner.txt?disposition=inline"))

(def numbers {\J 10000
              \U 5000
              \L 1000
              \E 500
              \T 100
              \R 50
              \3 10
              \V 5
              \I 1})

(defn parse-number [in]
  (let [numseq (remove nil? (map numbers in))
        fix-row (fn [row]
                  (let [row-end (peek row)]
                    (loop [new-row row
                           new-rows []]
                      ;(println "==== fixrow ===>" new-row new-rows)
                      (cond (empty? new-row)
                            new-rows

                            (= (count new-row) 1)
                            (conj new-rows new-row)

                            (>= (first new-row) row-end) ; all of new-row must be smaller than next
                            (recur (subvec new-row 1) (conj new-rows [(first new-row)]))

                            :else (recur nil (conj new-rows new-row))))))]
    ;(println numseq)
    (loop [[next & ns] (rest numseq)
           cur (first numseq)
           row []
           rows []]
      ;(println cur next row rows)
      (cond (nil? cur)
            rows

            (nil? next)
            (into rows (fix-row (conj row cur)))

            (<= next cur) ; currently in row
            (recur ns next (conj row cur) rows)

            :else ; new row (< cur next)
            (let [new-next (first ns)]
              (recur (rest ns) new-next [] (into rows (fix-row (conj row cur next)))))))))

(defn check-number [n]
  (loop [[a & row-ends] (map peek n)]
    (cond (empty? row-ends) true
          (< a (first row-ends)) false
          :else (recur row-ends))))

(defn convert-number [in]
  (let [n (parse-number in)]
    (if (check-number n)
      (reduce (fn [running-sum row]
                (+ running-sum
                   (- (apply + row))
                   (* 2 (peek row))))
              0 n)
      0)))

(time (->> (str/split-lines data)
           (map convert-number)
           (reduce max)))
;;=> 70669

;;; ***** tester
(parse-number "TR3VI3") ;;=> [[100] [50] [10] [5 1 10]]
(parse-number "VIV") ;;=> [[5] [1 5]]
(parse-number "TUR33VIE") ;;=> [[100 5000] [50 50 10 10 5 1 500]]
(parse-number "JULEL") ;;=> [[10000] [5000] [1000] [500 1000]]
(map (comp check-number parse-number) ["R33VIE" "TUR33VIE" "3R" "JULEL" ]) ;;=> (true true true true)
(map (comp check-number parse-number) ["RTR33VIE" "RTE" "VIV"]) ;;=> (false false true)
(parse-number "LLLU3I3VII")
(convert-number "LLLU3I3VII")


(->> (str/split-lines testdata)
     (map #(str/split % #" = "))
     (remove #(= (convert-number (first %)) (puzzle/str->long (second %)))))
;;=> ()

;;; *****


; alfred sin løsning: hvert tall er slutten av en gruppe om det er større enn alle tall til høyre

(defn convert-number-2 [in]
  (let [numseq (mapv numbers in)]
    (loop [sortert-idx (sort-by numseq > (range (count numseq)))
           [n & ns] numseq
           i 0
           total 0
           group 0]
      ;(println i sortert-idx n ns)
      (cond (> i 200000)
            -1

            (nil? n) ; ferdig
            total

            (= i (first sortert-idx))  ; dette tallet er størst, altså ferdig med en gruppe
            (recur (doall (remove #(= % i) sortert-idx)) ns (inc i) (+ total (- n group)) 0)

            (and (> i 0) (> n (numseq (dec i)))) ; dette tallet er større enn forrige tall
            0

            :else
            (recur (doall (remove #(= % i) sortert-idx)) ns (inc i) total (+ group n))))))

(convert-number-2 (first (drop 3 (str/split-lines data))))

(time
 (->> (str/split-lines data)
      (map convert-number-2)
     ;(filter #(= (convert-number-2 %) -1))
     ;(take 100)
      (reduce max)
      ))