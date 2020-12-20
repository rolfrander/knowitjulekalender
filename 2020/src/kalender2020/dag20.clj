(ns kalender2020.day20
  (:require [clojure.string :as str]))

(set! *unchecked-math* false)
(set! *warn-on-reflection* true)


(def testdata "Athena
Demeter
Hades
Hades🎄Hypnos
Athena🎄Icarus
Hades🎄Nyx🎄Zagreus🎄Medusa
Athena🎄Orpheus
Athena🎄Icarus🎄Poseidon🎄Cerberus
Hades🎄Nyx🎄Zagreus
Athena🎄Icarus🎄Poseidon")

(def split #"🎄")

(defn parse [input]
  (->> input
       str/split-lines
       (map #(str/split % split))))

(defn superior-map [data]
  (->> data
       (map reverse)
       (into {} (map (fn [[f & r]] [f r])))))

(defn remove-retired-and-reverse [superiors]
  (->> (map (fn [[k v]]
              [k (loop [[direct-leader & other-superiors] v]
                   (if (or (nil? direct-leader) (contains? superiors direct-leader))
                     direct-leader
                     (recur other-superiors)))])
            superiors)
       (group-by second)
       (map (fn [[k v]] [k (map first v)]))
       (into {})))

(defn has-subordinates [tree name]
  (contains? tree name))

(defn slim-down
  ([tree] (first (slim-down tree nil)))
  ([tree start]
   (let [direct-subordinates (get tree start)
         sub-cnt (count direct-subordinates)]
     (if (= 0 sub-cnt)
       [nil start]
       (if (and (= 1 sub-cnt) (has-subordinates tree (first direct-subordinates)))
         (slim-down tree (first direct-subordinates))

         (let [[new-tree new-sub-list]
               (reduce (fn [[new-tree new-sub-list] old-sub]
                         (let [[sub-tree sub] (slim-down tree old-sub)]
                           [(into new-tree sub-tree)
                            (conj new-sub-list sub)]))
                       [{} []]
                       direct-subordinates)]
           ;(println "slim-down" start new-sub-list new-tree)
           [(assoc new-tree start new-sub-list) start]))))))



(defn diff-worker-manager
  ([tree] (inc (diff-worker-manager tree nil)))
  ([tree start]
   (if (has-subordinates tree start)
     (dec (reduce + (map #(diff-worker-manager tree %) (get tree start))))
     1)))

(defn slim-diff-worker-manager
  ([tree] (inc (diff-worker-manager tree nil)))
  ([tree start]
   (if-let [subordinates (get tree start)]
     (let [sub-count (reduce + (map #(diff-worker-manager tree %) subordinates))]
       (if (= 1 (count subordinates)) 
         sub-count
         (dec sub-count)))
     1)))


(defn to-string 
  ([tree] (to-string tree nil))
  ([tree start]
   (let [subtree (get tree start)]
     (if (nil? subtree)
       start
       (format "%s -> [%s]" (if (nil? start) "Julenissen" start)
               (str/join ", " (map #(to-string tree %) subtree)))))))

(def space "                                                                                                                  ")
(defn pretty-print-org 
  ([tree] (pretty-print-org tree nil 0))
  ([tree start indent]
   (printf "%s%s %d%n" (.subSequence space 0 indent) (or start "Julenissen") (diff-worker-manager tree start))
   (flush)
   (doseq [sub (get tree start)]
     (pretty-print-org tree sub (+ indent 4)))))

(def data (parse (slurp "https://julekalender-backend.knowit.no/challenges/20/attachments/elves.txt")))

(-> data
    ;(parse testdata)
    superior-map
    remove-retired-and-reverse
    ;slim-down
    slim-diff-worker-manager
    )

(into {"julenissen" '("foo" "bar")})

(map slim-down (remove-retired-and-reverse (superior-map (parse testdata)))
           "Hades")