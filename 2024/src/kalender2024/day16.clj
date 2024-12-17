(ns kalender2024.day16
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.io StringWriter Reader)
           java.util.zip.GZIPInputStream))

(defn slurp-gunzip
  "decompress data.
    input: gzipped data  which can be opened by io/input-stream.
    output: something which can be copied to by io/copy (e.g. filename ...)."
  [input output & opts]
  (with-open [input (-> input io/input-stream GZIPInputStream.)]
    (apply io/copy input output opts)))

(defn slurp-gunzip [url]
  (let [sw (java.io.StringWriter.)]
    (with-open [^java.io.Reader r (-> url io/input-stream GZIPInputStream. io/reader)]
      (io/copy r sw)
      (.toString sw))))

(def kugutt-alv "BBBBBBBBBBRRROOOOOOOOONNHHHHH")

(def testdata " *       *      **     *      * ** **
jjggsssjjssssssjjjjjsssssjjjjjjsjjggggssj")

(def data (slurp-gunzip "https://julekalender-backend.knowit.no/challenges/2024-16/files/el-paso_santa-cruz.txt.gz?disposition=inline"))

(def acceptable-combo-1 #{[\H \* \s]
                          [\H \* \j]
                          [\H \space \s]
                          [\H \space \j]
                          [\N \space \j]
                          [\O \* \j]
                          [\O \* \s]
                          [\O \space \j]
                          [\O \space \s]
                          [\R \* \j]
                          [\R \* \s]
                          [\R \space \j]
                          [\R \space \s]
                          [\B \* \j]
                          [\B \* \s]
                          [\B \space \j]
                          [\B \space \s]})

(def acceptable-combo-2 #{[\H \space \j]
                          [\N \space \j]
                          [\O \space \j]
                          [\R \space \j]
                          [\B \space \j]})

(let [[sviller underlag] (str/split-lines data)
      tell-sviller (fn [kroppsdel combo]
                     (count (->> combo
                                 (filter #(= (first %) kroppsdel))
                                 (filter #(= (second %) \*)))))
      check-offset (fn [offset]
                     (let [combo (map vector
                                      kugutt-alv
                                      (subs sviller offset)
                                      (subs underlag offset))]
                       (or (and (every? acceptable-combo-1 combo)
                                ; minst 1 sville under hodet
                                (> (tell-sviller \H combo) 0)
                                ; maks 1 sville under beina
                                (<= (tell-sviller \B combo) 1))
                           (every? acceptable-combo-2 combo))))]
  (->> (range (- (count underlag) (count kugutt-alv)))
       (filter check-offset)
       count))