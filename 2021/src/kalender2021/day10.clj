(ns kalender2021.day10)

;; A  B  C
;; 
;; D  E  F
;; 
;; G  H  I

;; generate all possible combinations, including illegal jumps
;; then remove combinations with illegal jumps

(defn combinations [length unused]
  (case length
    0 ['()]
    1 (mapv #(cons % nil) unused)
    (mapcat (fn [item] (map (fn [comb]
                            (cons item comb))
                          (combinations (dec length) (remove #(= % item) unused))))
          unused)))

(defn illegal-passing [from to pass s]
  ;; return true if "from" and "to" are adjacent and "pass" is later in s
  (loop [i-from -1
         i-to -1
         i-pass -1
         i 0
         [element & s] s]
    (if (or (>= i-pass 0) (nil? element))
      (and (or (= i-pass -1)    ; pass is not found or
               (> i-pass i-to)) ; pass comes later in the sequence
           (>= i-from 0)   ; found from
           (>= i-to 0)     ; found to
           (= 1 (Math/abs (- i-from i-to))))
      (condp = element
        from (recur i      i-to i-pass (inc i) s)
        to   (recur i-from i    i-pass (inc i) s)
        pass (recur i-from i-to i      (inc i) s)
             (recur i-from i-to i-pass (inc i) s)))))

(illegal-passing :i :g :h [:a :b :c :e :f :g :i])

(count
 (->>
  ; allways start with d, find 7 others
  (combinations 7 '(:a :b :c :e :f :g :h :i))
  ; first can't be F, as D-F will pass E
  (remove #(= (first %) :f))
  ; can't cross from corner to corner before E is taken
  (remove #(illegal-passing :a :c :b %))
  (remove #(illegal-passing :c :i :f %))
  (remove #(illegal-passing :i :g :h %))
  (remove #(illegal-passing :a :i :e %))
  (remove #(illegal-passing :g :c :e %))
  (remove #(illegal-passing :b :h :e %))
  ))

(cons :a '(:b :c))

(cons :c (remove #(= % :c) [:a :b :c :d :e :f]))