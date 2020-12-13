(ns kalender2020.dag13
  (:require [clojure.string :as str]))

(def testdata "csfgunqvmiotgixxqeexdnwrtrgftpafkqepkvwwscotfahzneobiipslnbmgyxxumdwxeymprtjrhapxqvguqazkwiorstwcjii")

(let [data (slurp "https://julekalender-backend.knowit.no/challenges/13/attachments/text.txt")
      cnt (int-array 26)
      a (int \a)]
  (clojure.string/join
   (remove #(let [l (- (int %) a)
                  i (aget cnt l)]
              (aset-int cnt l (inc i))
              (not= i l)) 
           (clojure.string/trim data))))
;; => "aebdhgcfijmqolpnvutkzsxryw"

