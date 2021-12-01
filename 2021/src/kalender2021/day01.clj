(ns kalender2021.day01)

(def test1 "ento trefirefem
")
(def test2 "sjufirenitrettentrettitretrettitre")

(def numbers
  [["femti" 50]
   ["førti" 40]
   ["tretti" 30]
   ["tjue" 20]
   ["nitten" 19]
   ["atten" 18]
   ["sytten" 17]
   ["seksten" 16]
   ["femten" 15]
   ["fjorten" 14]
   ["tretten" 13]
   ["tolv" 12]
   ["elleve" 11]
   ["ti" 10]
   ["ni" 9]
   ["åtte" 8]
   ["sju" 7]
   ["seks" 6]
   ["fem" 5]
   ["fire" 4]
   ["tre" 3]
   ["to" 2]
   ["en" 1]])

(def data
  (slurp
   "https://julekalender-backend.knowit.no/rails/active_storage/blobs/redirect/eyJfcmFpbHMiOnsibWVzc2FnZSI6IkJBaHBNdz09IiwiZXhwIjpudWxsLCJwdXIiOiJibG9iX2lkIn19--0af4f790dec929a13e3615fdac11667323ea1597/tall.txt"))

(defn sum-text-numbers [input]
  (loop [n 0
         sum 0]
    (if (< n (.length input))
      (if (Character/isSpace (.charAt input n))
        (recur (inc n) sum)
        (let [match (first (filter (fn [x] (.startsWith input (first x) n)) numbers))]
          (if (not match)
            (throw (RuntimeException. (str "didnt find any match for " (.substring input n (min (.length input) (+ n 10))))))
            (recur (+ n (.length (first match)))
                   (+ sum (second match))))))
      sum)))

(sum-text-numbers test1)
(sum-text-numbers test2)
(sum-text-numbers data)
