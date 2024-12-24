(ns kalender2024.day24
  (:require
   [clojure.java.io :as io]) 
  (:import
   [java.io ByteArrayInputStream InputStreamReader]))

(defn file->bytes [file]
  (with-open [xin (io/input-stream file)
              xout (java.io.ByteArrayOutputStream.)]
    (io/copy xin xout)
    (.toByteArray xout)))

(def data (file->bytes "https://julekalender-backend.knowit.no/challenges/2024-24/files/kryptert.txt?disposition=inline"))

(defn decode [^bytes bytes]
  (let [out (byte-array (count bytes))]
    (aset-byte out 0 (aget bytes 0))
    (let [decrypted (loop [i 1
                           lastval (aget bytes 0)]
                      (if (>= i (count out))
                        out
                        (let [cur (aget bytes i)
                              v (unchecked-byte (bit-xor lastval cur))]
                          (aset out i v)
                          (recur (inc i) v))))
          ]
      (with-open [is (ByteArrayInputStream. decrypted)
                  r (InputStreamReader. is)]
        (slurp r)))))

(println (decode data))