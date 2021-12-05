(ns kalender2021.day03)

(def testdata  "JJJJJNNJJNNJJJJJ")
(def testdata2 "JJJJJNNJJNNJJJJJNNNNNJJJJJJJJJNNJNNNN")


(defn parse [data]
  (let [v (->> data
               (map #(if (= % \J) 1 -1))
               (into (vector)))]
    [-1 v]))

(def data (slurp "https://julekalender-backend.knowit.no/rails/active_storage/blobs/redirect/eyJfcmFpbHMiOnsibWVzc2FnZSI6IkJBaHBOdz09IiwiZXhwIjpudWxsLCJwdXIiOiJibG9iX2lkIn19--31fa0c541c69eeb9063ccfc56e686f4768662004/input.txt?disposition=inline"))

;(parse testdata)

(defn sum-offset [offset v1 v2]
  (let [values (mapv + (second v1) (nthrest (second v2) offset))
        first-neutral (.indexOf values 0)]
    [first-neutral values]))

(time
 (let [;data testdata2
       d (parse data)
       l (count data)
       v-empty (vec (repeat (/ l 2) [-1 []]))
   ;; calculate doubles
       v-doubles
       (loop [i 1
         ; v[i] is the sum of all subvectors of length 2*i for each starting position of d
              v (assoc v-empty 1 (sum-offset 1 d d))]
         (if (>= (* 4 i) l)
           v
           (recur (* 2 i)
                  (assoc v (* 2 i) (sum-offset (* 2 i)
                                               (get v i)
                                               (get v i))))))
       highest (loop [i (dec (count v-doubles))]
                 (if (or (= i 0) (> (first (get v-doubles i)) -1))
                   i
                   (recur (dec i))))]
  ;(clojure.pprint/pprint v-doubles)
   (loop [v v-doubles
          cur-high highest
          add (/ highest 2)]
     ;(println cur-high add)
     (if (< add 1)
       ;[(* 2 highest) (first (get v highest))]
       [(* 2 cur-high) (first (get v cur-high))]
       (let [new (sum-offset (* 2 add)
                             (get v add)
                             (get v cur-high))]
         (if (> (first new) -1)
           (recur (assoc v (+ cur-high add) new)
                  (+ cur-high add)
                  (/ add 2))
           (recur v cur-high (/ add 2))))))))

(doseq [l testdata2] (println l))

(/ 31 2)
