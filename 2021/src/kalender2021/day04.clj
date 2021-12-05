(ns kalender2021.day04)

(letfn
 [(sail-north [x y steps]
              (if (= 0 steps)
                [x y :north (format "%s,%s" x y)]
                (if (and (= 0 (mod y 3))
                         (not= 0 (mod y 5)))
                  #(sail-east (inc x) y (dec steps))
                  #(sail-north x (inc y) (dec steps)))))
  (sail-east [x y steps]
             (if (= 0 steps)
               [x y :east (format "%s,%s" x y)]
               (if (and (= 0 (mod x 5))
                        (not= 0 (mod x 3)))
                 #(sail-north x (inc y) (dec steps))
                 #(sail-east (inc x) y (dec steps)))))]
  (trampoline sail-north 66666666666666666665 33333333333333333335 79)
  ;(trampoline sail-east 5 5 1)
  )
; 100000000000000000079
;x 66666666666666666665
;y 33333333333333333335

(format "%s,%s" 3333333333333333333333333333 444444444444444444444444444)

; etter 3eN steg er X = 2eN og Y=1eN

; etter 666*(n) steg er x 444(*n) og y 222(*n)
