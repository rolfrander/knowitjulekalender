(ns kalender2022.day02)

(def gift-data (slurp "https://julekalender-backend.knowit.no/challenges/2022-02/files/gaver.txt"))

(defn parse-gifts [in]
  (map #(nil? (re-find #"alv" %))
       (clojure.string/split-lines in)))

(def gifts (parse-gifts gift-data))

(defn verse-lines [giftcnt]
  (inc (if (<= giftcnt 3)
         1
         (- giftcnt 2))))

(loop [[g & gifts] gifts
       giftcnt 1
       result 0]
  (if (nil? g) result
      (let [verse-acc (+ result (verse-lines giftcnt))]
        ;(println verse-acc)
        (if g
          ;; uten "alv"
          (recur gifts (inc giftcnt) verse-acc)
          ;; med "alv"
          (recur gifts giftcnt verse-acc)))))