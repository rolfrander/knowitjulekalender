(ns kalender2021.day12
  (:require [clojure.string :as string]))

(def testdata "K Leker
-K Elektriske
--G Raspberry pi
--G Tamogochi
-K Mekaniske
--G Rubics cube
-K Møbler
--K Sofaer
-K Foto
--K Kamera
---G Nicon P100")

(def testdata2 "G Sony WH-3000X
G Playstation 1
K Støvsugere
G Rubics cube
K Programvarer
-K PC Skjermer
--G Playstation 1
--K Kontorstoler
---K Kamera
--K Puter
G Raspberry pi")

(def data (slurp "https://julekalender-backend.knowit.no/rails/active_storage/blobs/redirect/eyJfcmFpbHMiOnsibWVzc2FnZSI6IkJBaHBBcklDIiwiZXhwIjpudWxsLCJwdXIiOiJibG9iX2lkIn19--6b8096adf032d85569a684cbda764a8cb8eba185/task.txt?disposition=inline"))

(loop [[l & linjer] (string/split-lines testdata2)
       kategorier 0
       tell-kategori false
       dybde -1]
  (if (nil? l)
    (+ kategorier (if tell-kategori (inc dybde) 0))
    
    (let [[_ prefix type] (re-matches #"^(-*)([GK]).*" l)
          ny-dybde (count prefix)]
      (println l prefix type kategorier tell-kategori dybde ny-dybde)
      (if (= type "K")
        (cond ;; kategori
          (= ny-dybde (inc dybde)) (recur linjer kategorier false (inc dybde))

          (= ny-dybde dybde) (recur linjer (if tell-kategori (inc kategorier) kategorier) false dybde)

          :else (recur linjer
                       (+ kategorier (if tell-kategori (- dybde ny-dybde) 0))
                       false ny-dybde))
        (cond
          (= ny-dybde (inc dybde)) (recur linjer kategorier true dybde)
          
          (<= ny-dybde dybde) (recur linjer (+ kategorier (if tell-kategori
                                                            (- dybde (dec ny-dybde))
                                                            0))
                                     true (dec ny-dybde))
          :else (println l dybde ny-dybde))))))


(defn parse [data]
  (letfn [(parse-internal [linjer dybde]
            (loop [[l & restlinjer] linjer
                   collect []]
              (if (nil? l)
                [nil collect]
                (let [[_ prefix type] (re-matches #"^(-*)([GK]).*" l)
                      ny-dybde (count prefix)]
                  (cond (> ny-dybde dybde)
                        (let [[retur-linjer retur-data] (parse-internal (conj restlinjer l) ny-dybde)]
                          (recur retur-linjer
                                 (conj collect retur-data)))

                        (= ny-dybde dybde)
                        (recur restlinjer
                               (if (= type "G") (conj collect true) collect)
                               ;(conj collect l)
                               )

                        :else
                        [(conj restlinjer l) collect])))))]

    (->> (parse-internal (string/split-lines data) -1)
         second ;; only collect-part
         first  ;; remove outer
         ))
  )



(defn tell-kateogrier [kategori]
  (let [[sub-count gifts] (reduce (fn [[sub-count gifts] next]
                                    (if (coll? next)
                                      [(+ sub-count (tell-kateogrier next)) gifts]
                                      [sub-count true]))
                                  [0 false]
                                  kategori)]
    (cond (> sub-count 0) (inc sub-count)
          (true? gifts) 1
          :else 0)))
  
(coll? [true])

(tell-kateogrier
 (parse testdata))