(ns dag04.core
  (:gen-class))

(def testdata "sukker: 24, mel: 20, egg: 17
sukker: 25, mel: 15, egg: 17
sukker: 17, melk: 14
sukker: 17, melk: 18
sukker: 25, melk: 24, egg: 10")

(defn sanitize-and-split [input]
     (->
      input
      (clojure.string/replace ":" "")
      (clojure.string/split #"[\n\r]+")
      ))

(defn read-as-assoc [list-of-strings]
  (map #(read-string (str "{" % "}")) list-of-strings))

(defn sum-maps [acc element]
  {'sukker (+ (acc 'sukker 0) (element 'sukker 0))
   'melk   (+ (acc 'melk 0)   (element 'melk 0))
   'mel    (+ (acc 'mel 0)    (element 'mel 0))
   'egg    (+ (acc 'egg 0)    (element 'egg 0))}
  )

(defn kaker [{sukker 'sukker
              mel 'mel
              melk 'melk
              egg 'egg}]
  (int (min (/ sukker 2)
            (/ mel 3)
            (/ melk 3)
            egg)))


(let [data (slurp "https://julekalender-backend.knowit.no/challenges/4/attachments/leveringsliste.txt")]
  (time (->> data
             sanitize-and-split
             read-as-assoc
             (reduce sum-maps {})
             kaker)))

(->> "https://julekalender-backend.knowit.no/challenges/4/attachments/leveringsliste.txt"
     slurp
     sanitize-and-split
     read-as-assoc)