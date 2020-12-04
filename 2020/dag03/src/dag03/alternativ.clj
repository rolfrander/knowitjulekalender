(ns dag03.alternativ)

(def s (slurp "matrix.txt"))

(def lines (map clojure.string/trim (clojure.string/split s #"\n")))

(def dict (clojure.string/split-lines (slurp "ordliste.txt")))

(def lc (count lines))

(def va (vec (map vec lines)))

(defn diags1 [va] 
  (for [y (range (- lc) lc 1)]
    (clojure.string/join (for [x (range lc)] 
                           (get-in va [(+ x y) x])))))

(defn diags2 [va]
  (for [y (range (- lc) lc 1)] 
    (clojure.string/join (for [x (range lc)] 
                           (get-in va [(+ x y) (- lc x)])))))

(defn verts [va] (for [y (range lc)] (clojure.string/join (for [x (range lc)] (get-in va [x y])))))

(def al (set (concat lines (verts va) (diags1 va) (diags2 va))))

(defn found? [al w] (or (not-empty (filter #(clojure.string/includes? %1 w) al)) (not-empty (filter #(clojure.string/includes? (clojure.string/reverse %1) w) al))))

(def m (remove #(found? al %1) dict))

(println "Check three-somes: " (filter #(= 3 (count %1)) al)) (println "Missings: " (clojure.string/join "," (sort m)))