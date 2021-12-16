(ns kalender2021.day16
  (:require [clojure.string :as string]))

(set! *warn-on-reflection* true)
(set! *unchecked-math*  :warn-on-boxed)

(def test-hist (slurp "https://julekalender-backend.knowit.no/rails/active_storage/blobs/redirect/eyJfcmFpbHMiOnsibWVzc2FnZSI6IkJBaHBBc0VDIiwiZXhwIjpudWxsLCJwdXIiOiJibG9iX2lkIn19--2285eaa85450c1fe0d89914a4754ca47ece0e42f/str%C3%B8mpriser_eksempel.txt?disposition=inline"))
(def test-next (slurp "https://julekalender-backend.knowit.no/rails/active_storage/blobs/redirect/eyJfcmFpbHMiOnsibWVzc2FnZSI6IkJBaHBBc0lDIiwiZXhwIjpudWxsLCJwdXIiOiJibG9iX2lkIn19--b78ea2c80b3b95f42f27e1795fe586918a664fdb/str%C3%B8mpriser_eksempel_next.txt?disposition=inline"))

(def data-hist (slurp "https://julekalender-backend.knowit.no/rails/active_storage/blobs/redirect/eyJfcmFpbHMiOnsibWVzc2FnZSI6IkJBaHBBc01DIiwiZXhwIjpudWxsLCJwdXIiOiJibG9iX2lkIn19--da41666bbf046e85ca348e28eecdbac89b6e4a67/str%C3%B8mpriser.txt?disposition=inline"))
(def data-next (slurp "https://julekalender-backend.knowit.no/rails/active_storage/blobs/redirect/eyJfcmFpbHMiOnsibWVzc2FnZSI6IkJBaHBBc1FDIiwiZXhwIjpudWxsLCJwdXIiOiJibG9iX2lkIn19--36c5a9fb6cefb1e71af8290bea926503f67f955f/str%C3%B8mpriser_next.txt?disposition=inline"))

(defn parse [input]
  (let [lines (vec (reverse (string/split-lines input)))
        price (fn [& l] 
                (first (keep-indexed #(when (not (= \space %2)) %1)
                                     l)))]
    (vec (apply map price lines))))

(defn sum-per-hour [data]
  (->> (partition 24 data)
       (apply map +)
       vec))

(defn cheapest-hour [aggregated]
  (apply min-key aggregated (range 24)))

(defn strategy-1 [data hour]
  (-> (sum-per-hour data)
      (get hour)))

(defn strategy-2 [hist next hour]
  (->> (loop [check-hour hour
              coll []]
         (cond (> check-hour (count hist))
               coll

               (or (>= (quot check-hour 24) 365)
                   (> (hist (+ check-hour 24))
                      (hist check-hour)))
               (recur (+ check-hour 48) (conj coll
                                              check-hour 
                                              (inc check-hour)))

               :else
               (recur (+ check-hour 24) (conj coll check-hour))))
       (map next)
       (apply +)
       ))

(let [hist (parse data-hist)
      next (parse data-next)
      cheap (cheapest-hour (sum-per-hour hist))
      s1 (strategy-1 (sum-per-hour next) cheap)
      s2 (strategy-2 hist next cheap)]
  (if (> s2 s1)
    (println "1," (- s2 s1))
    (println "2," (- s1 s2))))


