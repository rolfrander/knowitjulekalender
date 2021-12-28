(ns kalender2021.core
  (:require [clojure.data.priority-map :refer [priority-map]]))

(defn dijkstra 
  "for all nodes, find the shortest path from source to destination. 
   weight-fn takes to paramenters, u and v, and returns the distance from u to v.
   neighbour-fn takes one parameter, v, and returns all neighbours of v.
   Both u and v are taken from nodes"
  [nodes source destination weight-fn neighbour-fn]
  (loop [dist (-> (reduce (fn [ret v] (assoc ret v 99999999)) {} nodes)
                  (assoc source 0))
         prev {}
         Q (into (priority-map) dist)]
    (if (empty? Q)
      [dist prev]
      (let [u (first (peek Q))
            Q (pop Q)]
        (when (Q u) (throw (RuntimeException. (str u " is still in Q"))))
        (if (= u destination)
          (dist u)
          (let [[new-dist new-prev new-q]
                (->> (filter Q (neighbour-fn u)) ; for each neighbour v of u, still in Q
                     (reduce (fn [[new-dist new-prev q] v]
                               (let [alt (+ (new-dist u) (weight-fn u v))] ; alt <- dist[u] + length(u,v)
                                 (if (< alt (new-dist v))                           ; if alt < dist[v]
                                   [(assoc new-dist v alt)                          ;   dist[v] <- alt
                                    (assoc new-prev v u)                            ;   prev[v] <- u
                                    (assoc q v alt)]                        ;   Q.decrease_priority(v, alt)
                                   [new-dist new-prev q])))
                             [dist prev Q]))]
            (recur new-dist new-prev new-q)))))))