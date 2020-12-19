(ns kalender2020.day17
  (:require [clojure.string :as str]))

(set! *unchecked-math* true)
(set! *warn-on-reflection* true)

(def cleaner-data "
   sss  
  sssss 
 sssssss
 sssssss
 sssssss
  sssss 
   sss  ")

(def brush-data
  "kkk   kkk
kkkkkkkkk
kkkkkkkkk
 kkkkkkk 
 kkkkkkk 
 kkkkkkk 
kkkkkkkkk
kkkkkkkkk
kkk   kkk")

(def robot
  (let [parse (fn [data] (->> (for [[y line] (zipmap (range) (str/split-lines data))
                                    [x char] (zipmap (range) line)
                                    :when (not= char \space)]
                                [x y])
                              (into #{})))
        cleaner-coords (parse cleaner-data)
        brush-coords (parse brush-data)

        find-border (fn [data direction selector]
                    ;; direction=-1, selector=0 left border
                    ;; direction= 1, selector=0 right border
                    ;; direction=-1, selector=1 top border
                    ;; direction= 1, selector=1 bottom border
                      (let [shift (fn [pos] (update pos selector (partial + direction)))]
                        (filter (fn [pos]
                                  (not (contains? data (shift pos))))
                                data)))]
    {:cleaner 
     {:c cleaner-coords
      :l (find-border cleaner-coords -1 0)
      :r (find-border cleaner-coords  1 0)
      :t (find-border cleaner-coords -1 1)
      :b (find-border cleaner-coords  1 1)}
     :brush
     {:c brush-coords
      :l (find-border brush-coords -1 0)
      :r (find-border brush-coords  1 0)
      :t (find-border brush-coords -1 1)
      :b (find-border brush-coords  1 1)}
     }
    ))

;;; dimensjoner 4130 3485

(def width 4130)
(def height 3485)

(def data
      (with-open [in (clojure.java.io/reader "dag17.txt")]
        (let [ret (java.util.ArrayList. height)]
          (doseq [line (line-seq in)]
            (.add ret line))
          ret)))

(def floor
  (let [ret (java.util.ArrayList. height)]
    (dotimes [_i height]
      (.add ret (byte-array width (byte 0x00))))
    ret))

;(def floor (make-array Byte/TYPE height width))

(defn valid-position [points x y]
  (->> points
       (map (fn [[xx yy]] [(+ x xx) (+ y yy)]))
       (some (fn [[x y]] (= \x (.charAt ^String (.get ^java.util.ArrayList data y) x))))
       not))


(defn print-board [origo-x origo-y w h]
  (doseq [y (range origo-y (+ origo-y h))]
    (println (str/join (map (fn [data-char floor-byte]
                              (case data-char
                                \space (case floor-byte 0 \space 0x01 \. \?)
                                \x (case floor-byte 0 \x \X)
                                \!))
                            (.substring (.get data y) origo-x (+ origo-x w))
                            (for [x (range origo-x (+ origo-x w))] (aget (.get floor y) x)))))))

(defn clean-floor [points origo-x origo-y]
  (->> points
       (map (fn [[xx yy]] [(+ origo-x xx) (+ origo-y yy)]))
       (filter (fn [[x y]] (and (>= x 0) (>= y 0)
                                (< x width) (< y height)
                                (= \space (.charAt ^String (.get ^java.util.ArrayList data y) x)))))
       (reduce (fn [_debug [x y]] 
                 (aset-byte (.get ^java.util.ArrayList floor y) x (byte 0x01))
                 (inc _debug)
                 )
               0)
       )
  )



(time (dotimes [i 5000]
        (valid-position (get (:cleaner robot) :r) 6 14)))

(clean-floor (get (:brush robot) :c) 2 0)
(print-board 350 8 50 20)

(aget (.get floor 5) 8)

(defn clean-row [y]
  (loop [x 0
         last-position-valid false
         clean-count 0]
    (if (< x (- width 9))
      (let [border (if last-position-valid :r :c)]
        (if (valid-position (get (:cleaner robot) border) x y)
          (do (clean-floor (get (:brush robot) border) x y)
              (recur (inc x) true (inc clean-count)))
          (recur (inc x) false clean-count))))))

(time 
 (doseq [y (range 0 (- height 9))]
   (clean-row y)
))

(time
 (doall 
  (pmap (fn [start-y]
         (doseq [y (range start-y (min (+ start-y 50) (- height 9)))]
           (clean-row y)))
       (range 0 height 50))))


;; antall rene ruter
(reduce + (for [row floor
                status row
                ]
            status))
;; => sum når bare børsten går inn til veggen     2348796
;; => sum når støvsugeren går helt inn til veggen 2349087

;; antall skitne ruter
(reduce + (for [row data
                cell row
                :when (= \space cell)]
            1))
;; => 2637990

(- 2637990 2348796)
;; => 289194

(- 2637990 2349087)
;; => 288903

(/ (* width height 81) 2100000000.0)
