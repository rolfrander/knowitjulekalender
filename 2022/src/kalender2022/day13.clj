(ns kalender2022.day13)

(def kuler [[0.04 10]
            [0.04 15]
            [0.02 30]
            [0.05 15]])

(let [h (- 18 2)
      r 5
      l (Math/sqrt (+ (* h h) (* r r)))
      a (* Math/PI r l)]
  (->> (map #(apply * a %) kuler)
       (reduce +)))
