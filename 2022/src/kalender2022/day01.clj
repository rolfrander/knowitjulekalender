(ns kalender2022.day01)


(defn parse [input]
  (let [grouped (->> (re-seq #"([a-zA-Z]+),([a-zA-Z']+)" input)
                     (map rest)
                     (group-by #(subs (first %) 0 2)))]
    (->> (keys grouped)
         (reduce #(assoc %1 %2 (sort-by (comp count first) > (get grouped %2)))
                 {}))))

(def wishlist (slurp "https://julekalender-backend.knowit.no/challenges/2022-01/files/letter.txt"))

(def words (slurp "https://julekalender-backend.knowit.no/challenges/2022-01/files/dictionary.txt"))

(def wordlist (parse words))

(re-seq #"[^a-zA-Z,\n]" words)

(clojure.pprint/pprint
 (get (->> (re-seq #"([a-z]+),([a-zA-Z]+)" words)
           (map rest)
           (group-by #(subs (first %) 0 2))
           )
      "in"))


(defn translate-count [wishlist i]
  (if (= i (count wishlist))
    [0]
    (let [prefix (subs wishlist i (+ i 2))
          dict (get wordlist prefix)
          space (- (count wishlist) i)
          matches (filter (fn [[shona _english]]
                            (and (<= (count shona) space)
                                 (= shona (subs wishlist i (+ i (count shona))))))
                          dict)]
      (if (nil? matches)
        [] 
        (flatten (map (fn [[shona english]]
                        (let [subresults (translate-count wishlist (+ i (count shona)))
                              result-len (inc (count english)) ; +1 for space
                              ]
                          ;(printf (str "%" (inc i) "s" english "\n") "")
                          (map (partial + result-len) subresults)))
                      matches))))))

(translate-count "inichidomunhuimbwa" 0)
(translate-count wishlist 0)

(let [translated
      (loop [i 0
             result []]
        (if (>= (+ i 2) (count wishlist))
          result
          (let [prefix (subs wishlist i (+ i 2))
                dict (get wordlist prefix)
                space (- (count wishlist) i)
                [word translated] (some (fn [[key val]]
                                          (when (and (<= (count key) space)
                                                     (= key (subs wishlist i (+ i (count key)))))
                                            [key val]))
                                        dict)
                ]
            (if (nil? word) (do (println "error" [result i])
                                nil)
                (recur (+ i (count word)) (conj result translated)))
            )))]
  (+ -1 (count translated) (reduce #(+ %1 (count %2)) 0 translated)))

