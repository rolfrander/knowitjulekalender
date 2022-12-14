(ns kalender2022.day14)

(def ^:const w_p 10) ; vekt av pakker som leveres i hvert skift, * 100 kg
(def ^:const w_santa 10) ; vekt av slede+nisse, * 100 kg
(def ^:const w_deer 1) ; vekt pr reinsdyr, * 100 kg
(def ^:const pull 2) ; hvor mye vekt et reinsdyr kan trekke, * 100 kg

(loop [i 100
       w_pkg_total 0N
       deer 0N
       w_deer_total 0N]
  ;(println i w_pkg_total deer w_deer_total)
  (if (= i 0)
    deer
    (let [w_pkg_total (+ w_pkg_total w_p)
          w_total (+ w_pkg_total w_deer_total w_santa)
          deer (+ deer (quot w_total pull) (mod w_total pull))]
      (recur (dec i)
             w_pkg_total
             deer
             (* deer w_deer)))))
