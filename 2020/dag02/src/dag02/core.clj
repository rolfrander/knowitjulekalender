(ns dag02.core
  (:gen-class))

(defn parse-line [str-line]
  (->> (str/split str-line #" " )
       (map #(Integer/parseInt %))))

(defn load-primes [filename]
  (with-open [rdr (io/reader filename)]
    (let [data (line-seq rdr)]
      (int-array (mapcat parse-line data)))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


