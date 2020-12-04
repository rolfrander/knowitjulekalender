(ns dag03.core
  (:gen-class))

(defn load-data [filename]
  (with-open [in (clojure.java.io/reader filename)]
    (reduce conj [] (line-seq in))))


(def data
  (load-data "matrix.txt"))

(def ordliste (load-data "ordliste.txt"))

(defrecord direction [^int x
                      ^int y])

(defrecord position [^int x
                     ^int y])

(def east       (direction.  1  0))
(def west       (direction. -1  0))
(def south      (direction.  0  1))
(def north      (direction.  0 -1))
(def south-east (direction.  1  1))
(def south-west (direction. -1  1))
(def north-east (direction.  1 -1))
(def north-west (direction. -1 -1))

(def all-directions (list east west north south south-east south-west north-east north-west))

(defn move [pos 
            dir
            width
            height]
  (let [x (int (+ (:x pos) (:x dir)))
        y (int (+ (:y pos) (:y dir)))
        w (int width)
        h (int height)]
    (if (or (< x 0) (>= x w)
            (< y 0) (>= y h))
      nil
      (position. x y))))

(defn move-n [pos
              dir
              distance]
  (let [n (int distance)
        x (+ (:x pos) (* n (:x dir)))
        y (+ (:y pos) (* n (:y dir)))
        ]
    (if (or (< x 0) 
            (< y 0) )
      nil
      (position. x y))))

(defn nth-2d [data
              pos]
  (get-in data [(:y pos) (:x pos)]))

(defn index [data letters]
  (let [idx (reduce (fn [output letter] (assoc output letter nil))
                    nil letters)]
    (reduce-kv (fn [output y ^String line]
                 (loop [x 0
                        collect output]
                   (if (< x (.length line))
                     (let [letter (nth line x)]
                       (recur (inc x)  (if (contains? collect letter)
                                         (update collect letter conj (position. x y))
                                         collect)))
                     collect)))
               idx data)))

(defn match-from-position [data ^String word position direction]
  ;; for each iteration: 
  ;;   if i == len(word) return (word position direction)
  ;;   compare word[i] with letter at position
  ;;     if match, loop, moving one in direction
  ;;     else return nil
  (let [word-len (int (.length word))
        head-row ^String (first data)
        w (int (.length head-row))
        h (int (count data))]
    (loop [p position
           i 0]
      (if (= i word-len)
        (list word position direction)
        (if p
          (if (= (nth word i)
                 (nth-2d data p))
            (recur (move p direction w h) (inc i))
            nil))))))

(defn indexed-search-from-positions [data words positions]
  ; for hver posisjon
  ;  - for hvert ord
  ;     - for hver retning (s, se, e, ne, n, osv)
  ;        - finnes w i denne posisjonen og i denne retningen?
  ;           => samle w, pos og retning 
  (for [p positions
        :let [pos-match
              (for [w words
                    :let [word-match
                          (for [d all-directions
                                :let [match (match-from-position data w p d)]
                                :when match]
                            match)]
                    :when (not-empty word-match)]
                word-match)]
        :when (not-empty pos-match)]
    pos-match))

(defn make-word-idx [words]
  (reduce (fn [idx word] (update idx (first word) conj word))
          nil words))


(defn indexed-search [data words]
  (let [word-idx   (time (doall (make-word-idx words)))
        letters    (time (doall (keys word-idx)))
        letter-idx (time (doall (index data letters)))]
    (mapcat identity 
            (for [l letters
                  :let [words (get word-idx l)
                        positions (get letter-idx l)]]
              (do (print l)
                  (time (doall (indexed-search-from-positions data words positions))))))))

(comment

(nth-2d data (position. 45 67))
(get-in data [-67 45])

(clojure.string/join '(\a \b nil \c \d))
  
(let [words ["julestjerne" "julebrus" "jesusbarnet" "juletre" "julestrømpe"]
      positions (list (position. 865 734)
                      (position. 798 657)
                      (position. 690 476)
                      (position. 439 192))]
  (time  (doall (indexed-search-from-positions data words positions))))
  
(set! *warn-on-reflection* true)

(time (do 
        (doall (index data (keys (make-word-idx ordliste))))
        nil))

(time (do (def result (doall (indexed-search data ordliste)))
          nil))

(let [result (time (doall (indexed-search data ordliste)))
      destructured-result (seq (doall (for [element result
                                            :let [word (first (first (first element)))]]
                                        word)))]
  (sort (for [o ordliste
              :when (not (some #(= o %) destructured-result))]
          o)))

(indexed-search-from-positions data ordliste (list (position. 480 390) (position. 798 657)))

(indexed-search-from-positions ["qwerasdfzxcv" "asdfasdfqwer" "zxcvzxcvqwer"] ["ws" "sd" "cx" "ww"] (list (position. 1 0)))

(let [ordliste ["klementin" "juletre" "marsipangris" "pinnekjøtt" "askepott"]]
  (loop [i 0
         result []]
    (if (>= i (count ordliste))
      result
      (let [ord (nth ordliste i)]
        (let [treff (search-all data ord)]
          (if treff
            (do (prn "fant " ord treff) (recur (inc i) result))
            (do (prn "fant ikke " ord) (recur (inc i) (conj result ord)))))))))


(time
 (kmp-table "participate in parachute "))

(time
 (dotimes [n 10000]
   (kmp-table "julestjerne")))

(time (kmp-search-2d [testvektor testvektor] "julestjerne" east (position. 0 0)))

(time
 (dotimes [n 100]
   (kmp-search testvektor "julestjerne")))

(time (clojure.string/includes? testvektor "julestjerne"))

)
;(nth (nth data 1) 3)

; (-> (java.io.File. ".") .getAbsolutePath)



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

