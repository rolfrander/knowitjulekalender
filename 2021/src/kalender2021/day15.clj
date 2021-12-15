(ns kalender2021.day15
  (:require [clojure.string :as string]))

(def alphabet "abcdefghijklmnopqrstuvwxyzæøå")
(def encode-char (into {} (map vector alphabet (rest (range)))))
(def decode-int (into {} (map vector (rest (range)) alphabet)))

(defn pad-and-encode [input]
  (loop [i 0
         [x & tail] input
         output []]
    (if (and (= 0 (mod i 8)) (nil? x))
      output
      (recur (inc i)
             tail
             (conj output (encode-char (or x \x)))))))

(defn decode [input]
  (string/join (map decode-int input)))

(defn rot- [a b]
  (loop [sum (- b a)]
    (if (> sum 0)
      sum
      (recur (+ sum 29)))))

(defn decrypt [key ciphertext]
  (let [encoded-key (pad-and-encode key)
        magic-n (count key)]
    (->> ciphertext
         pad-and-encode
         (map-indexed #(rot- (* magic-n (inc (quot %1 8))) %2))
         (map-indexed #(rot- (inc (mod %1 8)) %2))
         (map rot- (cycle encoded-key))
         decode)))

(def c1 "wawwgjlmwkafeosjoæiralop")
(def c2 "jagwfjsuokosjpzæynzxtxfnbæjkæalektfamxæø")
(def c3 "wawwgjlmwkoåeosaæeoltååøbupscpfzqehkgdhkjdoqqkuuakvwogjkpøjsbmpq")
(def c4 "vttyøyønøbjåiåzpejsimøldajjecnbplåkyrsliænhbgkvbecvdscxømrvåmagdioftvivwøkvbnyøå")
(def all-c [c1 c2 c3 c4])
(map (partial decrypt "alvalv") all-c)


(defn rot+ [a b]
  (loop [sum (+ a b)]
    (if (<= sum 29)
      sum
      (recur (- sum 29)))))

(defn encrypt [key cleartext]
  (let [encoded-key (pad-and-encode key)
        magic-n (count key)]
    (->> cleartext
         pad-and-encode
         (map rot+ (cycle encoded-key))
         (map-indexed #(rot+ (inc (mod %1 8)) %2))
         (map-indexed #(rot+ (* magic-n (inc (quot %1 8))) %2))
         decode)))

(defn partial-decrypt [assumed-key-length ciphertext]
  (let [magic-n assumed-key-length]
    (->> ciphertext
         pad-and-encode
         (map-indexed #(rot- (* magic-n (inc (quot %1 8))) %2))
         (map-indexed #(rot- (inc (mod %1 8)) %2))
         ;(map rot2 encoded-key)
         decode)))

(def extra [6 5 6 6])
(letfn [(right [string] (.substring string (- (.length string) 8)))
        (rr-decrypt [c] (-> (partial-decrypt 6 c);(decrypt "x" c)
                            ))]
  (->> (map rr-decrypt all-c)
       (map right)
       (map (fn [c] (decode (map #(rot- %1 %2) (pad-and-encode "x") (pad-and-encode c)))))
       ;(map #(.substring %2 0 (- (.length %2) %1)) extra)
       ))

