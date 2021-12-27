(ns kalender2021.day19
  (:require [clojure.string :as string]
            [clojure.data.priority-map :refer [priority-map]]))

(def testdata (string/split-lines "09:52, Badeand, 1, 7, Presskanne, 2, 4, Stekepanne, 2, 5
09:55, Bok, 2, 5, Longboard, 1, 7, Surfebrett, 10, 25"))

(def data (string/split-lines (slurp "https://julekalender-backend.knowit.no/rails/active_storage/blobs/redirect/eyJfcmFpbHMiOnsibWVzc2FnZSI6IkJBaHBBc3dDIiwiZXhwIjpudWxsLCJwdXIiOiJibG9iX2lkIn19--bb0aa747ae7259b673eb90e9e7c024958ebfbfd6/factory.txt?disposition=inline")))

(defn parse-line [line]
  (let [[h-str m-str & gaver] (re-seq #"[^:, ]+" line)
        h (Long/parseLong h-str)
        m (Long/parseLong m-str)]
    (conj (->> (partition 3 gaver)
               (map (fn [[navn lagetid pakketid]]
                      [navn (Long/parseLong lagetid) (Long/parseLong pakketid)])))
          (+ m (* 60 h)))))

(defn parse [input]
  (map parse-line input))

;(parse testdata)

(defn produce-time [gift] (second gift))

(defn packing-time [gift] (nth gift 2))

(defn gift-name [gift] (first gift))

(let [d (parse data)
      factory-start (mapv (fn [[start-time & products]]
                            {:current (first products)
                             :finish-time (+ start-time (produce-time (first products)))
                             :queue (rest products)}) d)
      cycle-production-machine (fn [factory machine-index current-time]
                                 (update factory machine-index #(let [next-gift (first (:queue %))]
                                                                  (if (nil? next-gift)
                                                                    nil
                                                                    {:current next-gift
                                                                     :finish-time (+ current-time (produce-time next-gift))
                                                                     :queue (rest (:queue %))}))))
      current-package (fn [factory machine-index] (:current (factory machine-index)))
      next-finish-time (fn [factory machine-index] (:finish-time (factory machine-index)))]
  (loop [current-time 0
         factory-state factory-start
         produce (into (priority-map) (map-indexed (fn [factory-index production-machine]
                                                     [factory-index (:finish-time production-machine)])
                                                   factory-state))
         packing (priority-map)
         max-packing-stations 0
         packing-order 0]
    (if (empty? produce)
      max-packing-stations
      (let [[machine-index production-done-time] (peek produce)
            next-packing-ready (peek packing)]
        (if (or (nil? next-packing-ready) (<= production-done-time
                                              (second next-packing-ready)))
          (let [current-time production-done-time
                gift-ready-for-packing (current-package factory-state machine-index)
                updated-factory (cycle-production-machine factory-state machine-index current-time)
                new-product-ready-time (next-finish-time updated-factory machine-index)
                updated-packing (assoc (if (= current-time (second next-packing-ready))
                                         (pop packing)
                                         packing)
                                       (format "%s (%d)" (gift-name gift-ready-for-packing) packing-order)
                                       (+ current-time (packing-time gift-ready-for-packing)))]
            (comment println (quot current-time 60) (mod current-time 60) "packing"
                     ;gift-ready-for-packing
                     updated-packing
                     ;(quot current-time 60) (mod current-time 60) "producing" (map :current updated-factory)
                     )
            (recur current-time
                   updated-factory
                   (if (nil? new-product-ready-time)
                     (pop produce)
                     (assoc (pop produce) machine-index new-product-ready-time))
                   updated-packing
                   (max max-packing-stations (count updated-packing))
                   (inc packing-order)))
          (let [[_packing-order current-time] next-packing-ready]
            (recur current-time
                   factory-state
                   produce
                   (pop packing)
                   max-packing-stations
                   packing-order)))))))

