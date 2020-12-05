(ns dag03.kmp)


(defn kmp-table ^ints [word]
  (let [len (.length word)
        t  (int-array (inc len))]
    (aset-int t 0 -1)
    (loop [pos 1
           cnd 0]
      (if (< pos len)
        (let [new-cnd (if (= (nth word pos)
                             (nth word cnd))
                        (do (aset-int t pos (aget t cnd))
                            cnd)
                        (do
                          (aset-int t pos cnd)
                          (loop [cnd (aget t cnd)]
                            (if (and (>= cnd 0) (not= (nth word pos)
                                                      (nth word cnd)))
                              (recur (aget t cnd))
                              cnd))))]
          (recur (inc pos) (inc new-cnd)))
        (aset-int t pos cnd)))
    t))

(defn kmp-search [s word]
  (let [t (kmp-table word)
        len-s (.length s)
        len-w (.length word)]
    (loop [j 0
           k 0]
      (if (< j len-s)
        (if (= (nth word k)
               (nth s j))
          (if (= (inc k) len-w)
            (- j k)
            (recur (inc j) (inc k)))
          (let [kk (aget t k)]
            (if (< kk 0)
              (recur (inc j) (inc kk))
              (recur j kk))))))))

(defn kmp-search-2d [^clojure.lang.PersistentVector s
                     ^String word
                     ^direction dir
                     ^position start]
  (let [t (kmp-table word)
        width (.length (first s))
        len-w (.length word)
        height (count s)]
    (loop [j ^position start
           k 0]
      (if j
        (if (= (nth word k)
               (nth-2d s j))
          (if (= (inc k) len-w)
            (move-n j dir (- k))
            (recur (move j dir width height) (inc k)))
          (let [kk (aget t k)]
            (if (< kk 0)
              (recur (move j dir width height) (inc kk))
              (recur j kk))))))))

(defn search-down [data word dir]
  (loop [y 0]
    (if (< y (count data))
      (let [res (kmp-search-2d data word dir (position. 0 y))]
        (if res res
            (recur (inc y)))))))

(defn search-across [data word dir]
  (loop [x 0]
    (if (< x (.length (first data)))
      (let [res (kmp-search-2d data word dir (position. x 0))]
        (if res res
            (recur (inc x)))))))

(defn search-diag [data word dir]
  (or (search-across data word dir)
      (search-down data word dir)))

(defn cond-and-join [test label]
  (if test
    (list test label)
    nil))

(defn search-all [data word]
  (let [rev (clojure.string/reverse word)]
    (or (cond-and-join (search-down   data word east) "east")
        (cond-and-join (search-across data word south) "south")
        (cond-and-join (search-diag   data word south-east) "south-east")
        (cond-and-join (search-diag   data word south-west) "south-west")
        (cond-and-join (search-down   data rev  east) "west")
        (cond-and-join (search-across data rev  south) "north")
        (cond-and-join (search-diag   data rev  south-east) "north-west")
        (cond-and-join (search-diag   data rev  south-west) "north-east"))))
