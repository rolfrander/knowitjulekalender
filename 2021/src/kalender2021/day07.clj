(ns kalender2021.day07)

; for hver runde er strikken l = 20n lang
; mauren beveger seg 1 / l del av strikken
; for hvilken n er sum 1/(20n) = 1
; altså sum 1/n = 20

(time
 (loop [maur (double 1.0)
        strikk (double 20)]
   (let [strikk2 (+ strikk 20.0)
         maur2 (+ 1.0 (* maur (/ strikk2 strikk)))]
     (if (<= strikk2 140.0)
       (println maur2 strikk2))
     (if (>= maur2 strikk2)
       (long strikk2)
       (recur maur2 strikk2)))))

(time
 (loop [maur 1
        strikk 20]
   (let [strikk2 (+ strikk 20)
         maur2 (inc (* maur (/ strikk2 strikk)))]
     (if (<= strikk2 140)
       (println maur2 strikk2))
     (if (>= maur2 strikk2)
       (long strikk2)
       (recur maur2 strikk2)))))

(time
 (loop [maur 1.0
        steg 2.0]
   (if (>= maur 20.0)
     (long (* (dec steg) 20))
     (recur (+ maur (/ 1 steg))
            (inc steg)))))

(/ 738.0 331)
(/ 5448012000 20)

(let [a (long 30)
      b (long 40)]
  (/ a b))