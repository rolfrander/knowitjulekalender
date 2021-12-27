(ns kalender2021.day20
  [:require [clojure.string :as string]])

(def testdata (slurp "https://julekalender-backend.knowit.no/rails/active_storage/blobs/redirect/eyJfcmFpbHMiOnsibWVzc2FnZSI6IkJBaHBBczBDIiwiZXhwIjpudWxsLCJwdXIiOiJibG9iX2lkIn19--47811069646354bdd33b9d7e59d650b4479a8586/example.txt?disposition=inline"))
(def data (slurp "https://julekalender-backend.knowit.no/rails/active_storage/blobs/redirect/eyJfcmFpbHMiOnsibWVzc2FnZSI6IkJBaHBBczRDIiwiZXhwIjpudWxsLCJwdXIiOiJibG9iX2lkIn19--8c02ba82414b0b5862f18fc5899e5c855f788178/maze.txt?disposition=inline"))

(def north-bit 8)
(def east-bit 4)
(def south-bit 2)
(def west-bit 1)

; directions :n :w :s :w
(def left-wall
  {:n west-bit
   :e north-bit
   :s east-bit
   :w south-bit})

(def front-wall
  {:w west-bit
   :n north-bit
   :e east-bit
   :s south-bit})

(defn turn-left [state]
  (let [turns {:n :w
               :e :n
               :s :e
               :w :s}]
    (update state :direction turns)))

(defn turn-right [state]
  (let [turns {:n :e
               :e :s
               :s :w
               :w :n}]
    (update state :direction turns)))

(defn forward [state]
  (case (:direction state)
    :n (update-in state [:pos 0] dec)
    :e (update-in state [:pos 1] inc)
    :s (update-in state [:pos 0] inc)
    :w (update-in state [:pos 1] dec)
    ))

(def start-state {:pos [0 0]
                  :direction :s})

(defn has-left-wall [maze state]
  (= (bit-and (get-in maze (:pos state))
              (left-wall (:direction state)))
     0))

;(has-left-wall (parse testdata) (turn-right {:pos [0 1] :direction :e}))

(defn has-front-wall [maze state]
  (= (bit-and (get-in maze (:pos state))
              (front-wall (:direction state)))
     0))

(defn has-escaped [maze state]
  (and (= (inc (first (:pos state))) (count maze))
       (= (inc (second (:pos state))) (count (peek maze)))))


(defn parse-line [line]
  (letfn [(maybe [in bit] (if (= in "1") bit 0))
          (parse-coord [[_all n e s w]]
            (bit-or (maybe n north-bit)
                    (maybe e east-bit)
                    (maybe s south-bit)
                    (maybe w west-bit)))]
    (mapv parse-coord (re-seq #"\(([01]),([01]),([01]),([01])\)" line))))

(defn parse [input]
  (mapv parse-line (string/split-lines input)))

(defn step [maze state steps]
  (if (or (nil? state) (has-escaped maze state))
    [state steps]
    (if (has-left-wall maze state)
      (if (has-front-wall maze state)
        [(turn-right state) steps]
        [(forward state) (inc steps)])
      [(forward (turn-left state)) (inc steps)])))

(let [maze (parse data)]
  (loop [state start-state
         steps 0]
    (if (has-escaped maze state)
      steps
      (let [[next-state next-steps] (step maze state steps)]
        (recur next-state next-steps)))))

(let [maze (parse testdata)]
  (let [trail-coords (->> (iterate (fn [[state steps]] (step maze state steps)) [start-state 0])
                          (take-while #(not (has-escaped maze (first %))))
                          (map (comp :pos first))
                          (into #{}))]
    (doseq [line (range 10)]
      (doseq [pos (range 10)]
        (if (trail-coords [line pos])
          (print "X")
          (print ".")))
      (newline))))


(->> (lazy-seq (list start-state start-state))
     (map :pos))