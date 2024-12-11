(ns kalender2024.day07)

(defn siffer [tall]
  (loop [t tall
         s '()]
    (if (= 0 t)
      s
      (recur (quot t 10)
             (cons (mod t 10) s)))))

(defn siffer->long [siffer]
  (reduce #(+ (* %1 10) %2) siffer))


(let [mem (atom {})]
  (defn regel-1
    "sum av kvadrat av siffer blir til slutt 1"
    [tall]
    (if (= tall 0)
      true
      (letfn [(regel-internal [tall i]
                (if-let [e (find @mem tall)]
                  (val e)
                  (let [ret (if (= i 6) false
                              ; dette er selve algoritmen
                                (let [q (apply + (map #(* % %) (siffer tall)))]
                                  (if (= q 1) true
                                      (regel-internal q (inc i))))
                              ; ********
                                )]
                    (swap! mem assoc tall ret)
                    ret)))]
        (regel-internal tall 0)))))

(defn half-ceil [i]
  (+ (quot i 2) (mod i 2)))

(defn regel-2
  "dele tallet i to"
  [tall]
  (if (< tall 10)
    true
    (let [s (vec (siffer tall))
          first-half (siffer->long (subvec s 0 (half-ceil (count s))))
          second-half (siffer->long (subvec s (quot (count s) 2)))]
      (and (regel-1 first-half)
           (regel-1 second-half)))))

(defn regel-3
  "alle påfølgende tre siffer"
  [tall]
  (if (< tall 1000)
    true
    (let [s (siffer tall)
          tredeler (map siffer->long (partition 3 1 s))]
      (not (some #(not (regel-1 %)) tredeler)))))


(time
 (->> (range 9999999 0 -1)
      (filter regel-3)
      (filter regel-2)
      (filter regel-1)
      first))
