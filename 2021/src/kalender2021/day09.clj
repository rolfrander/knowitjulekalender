(ns kalender2021.day09)


; År	Pakker til overs	# Snille barn
; 2019	1854803357	2424154637
; 2020	2787141611	2807727397
; 2021	1159251923	2537380333

; x == o (mod b)

; pakker-til-overs
(def pakker-til-overs [1854803357
                       2787141611
                       1159251923])

;snille-barn
(def snille-barn [2424154637
                  2807727397
                  2537380333])

(defn euclid-gcd [x1 x2]
  (loop [a (max x1 x2)
         b (min x1 x2)]
    (if (= b 0)
      a
      (recur b (mod a b)))))

;(euclid-gcd 1071 462)

[(euclid-gcd (get n 1) (get n 2))
 (euclid-gcd (get n 1) (get n 0))
 (euclid-gcd (get n 2) (get n 0))]

;(/ (Math/log (Math/pow 10 13)) (Math/log 2))
;(format "%x" (long (Math/pow 10 13)))

;; pseudokode
;; i=1
;; off=n[0]
;; x=a[0]

;; while x mod n[i] != a[i]
;;   x = x+off
;; loop i=i+1, off=off*n[i]

(defn crt [a n]
  (loop [x (first a)
         [ai & a] (rest a)
         off (first n)
         [ni & n] (rest n)]
    (if (nil? ai)
      x
      (recur (loop [x x]
               (if (= (mod x ni) ai)
                 x
                 (recur (+ x off))))
             a
             (* off ni)
             n))))

(crt pakker-til-overs snille-barn)

(map #(mod 2515703161926 %) n)