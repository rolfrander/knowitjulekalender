(ns kalender2021.day21
  [:require [clojure.string :as string]])

(def ^:dynamic *debug* false)

(def words (->> (slurp "resources/wordlist.txt")
                string/split-lines
                (into [])))

(def alphabet "abcdefghijklmnopqrstuvwxyzæøå")
(def ord (into {} (map-indexed #(vector %2 %1) alphabet)))

(defn cmp [^String a ^String b]
  (letfn [(ord-idx [^String str idx]
            (if (>= idx (.length str))
              -1
              (ord (.charAt str idx))))]
    (loop [i 0]
      (let [la (ord-idx a i)
            lb (ord-idx b i)]
        (cond
          (> la lb) 1
          (< la lb) -1
          (= la -1) 0
          :else (recur (inc i)))))))

(defn find-prefix [prefix]
  (if (= prefix "")
    :prefix
    (letfn [(linear-search [lo hi]
              (reduce (fn [ret cur]
                        (when *debug* (println "linear:" (words cur)))
                        (if (= prefix (words cur))
                          (reduced :exact)
                          (if (or (= :prefix ret)
                                  (.startsWith (words cur) prefix))
                            :prefix
                            nil)))
                      nil
                      (range lo (inc hi))))]
      (loop [lo 0
             cur 16
             hi (dec (count words))]
        ;(println lo cur hi)
        (when *debug* (println lo (words lo) cur (words cur) hi (words hi)))
        (cond (<= (- hi lo) 32) (linear-search lo hi)

              (= (words cur) prefix) :exact

              (< (cmp (words cur) prefix) 0)
              (recur cur (quot (+ cur hi) 2) hi)

              :else
              (recur lo (quot (+ cur lo) 2) cur))))))

(binding [*debug* true]
  (find-prefix "ad"))

(def to-letter (->> (map-indexed #(vector (str %1) (str %2)) " abcdefghijklmnopqrstuvwxyzæøå")
                    rest
                    (into {})))

(def cipher "45205145192051057281419115181357209121021125181201516161911252091475141221011351923522729182181222718192919149121210211251491919514")

(def space " ")

;(defn i [n] (.subSequence space 0 (* 2 n)))
(defn i [n] (format (str "%" (inc n) "s") ""))

(defn decode-word
  ([chars]
   (decode-word [] chars 0))

  ([prefix chars indent]
   ;(printf "%sdecode-word %s %s\n" (i indent) prefix (find-prefix (string/join prefix)))
   (let [prefix-string (string/join prefix)
         ret
         (if-let [prefix-match (find-prefix prefix-string)]
           (do
             ;(println "valid prefix:" prefix-string prefix-match "followed by" (first chars))
             (if (empty? chars)
               (if (= prefix-match :exact) (list prefix-string) nil)
               (let [next-element (first chars)
                     eager-grab (if (or (vector? next-element) (seq? next-element))
                                  (some #(let [subseq (string/split % #"")]
                                           (decode-word (conj prefix (first subseq))
                                                        (concat (rest subseq) (rest chars))
                                                        (inc indent))) next-element)
                                  (decode-word (conj prefix next-element) (rest chars) (inc indent)))]
                 (if (nil? eager-grab)
                   (if (= :exact prefix-match)
                     (let [decode-rest (decode-word [] chars (inc indent))]
                       (if (nil? decode-rest)
                         nil
                         (conj decode-rest prefix-string)))
                     nil)
                   eager-grab))))
           nil)]
     ret)))

(decode-word '("g" "o" ["dj" "x"] "u" "l"))

(defn decode-numstring [[cipher special]]
  (letfn [(decode-special [^String s]
            (case (count s)
              1 (to-letter s)
              2 [(to-letter s) (str (to-letter (.subSequence s 0 1))
                                    (to-letter (.subSequence s 1 2)))]
                          ; default
              (let [first-1 (to-letter (.subSequence s 0 1))
                    rest-1 (decode-special (.subSequence s 1 (.length s)))
                    first-2 (to-letter (.subSequence s 0 2))
                    rest-2 (decode-special (.subSequence s 2 (.length s)))]
                (cond (nil? rest-1) (map #(str first-2 %) rest-2)
                      (nil? rest-2) (map #(str first-1 %) rest-1)
                      :else
                      (concat (map #(str first-1 %) rest-1)
                              (map #(str first-2 %) rest-2))))))]
    (if (nil? special)
      (to-letter cipher)
      (decode-special special))))

(defn decode [ciph]
  (->> (re-seq #"[3456789]|[12]0|([12]+[3456789]?)(?=[1-9]|$)" ciph)
       (map decode-numstring)
       ))

(string/join (decode-word (decode cipher)))


(decode "1491919514")

