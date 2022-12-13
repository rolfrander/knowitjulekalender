(ns kalender2022.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [exif-processor.core :as exif])
  (:import [com.drew.imaging ImageMetadataReader]))

(def exif-directory-regex
  (re-pattern (str "(?i)(" (string/join "|"
                                 ["Exif" "JPEG" "JFIF"
                                  "Agfa" "Canon" "Casio" "Epson"
                                  "Fujifilm" "Kodak" "Kyocera"
                                  "Leica" "Minolta" "Nikon" "Olympus"
                                  "Panasonic" "Pentax" "QuickTime" "Sanyo"
                                  "Sigma/Foveon" "Sony" "GPS"]) ")")))

(defn- extract-from-tag
  [tag]
  (into {} (map #(hash-map (.getTagName %) (.getDescription %)) tag)))

(defn exif-for-file
  "Takes an image file (as a java.io.InputStream or java.io.File) and extracts exif information into a map"
  [file]
  (let [metadata (ImageMetadataReader/readMetadata file)
        exif-directories (.getDirectories metadata)
        tags (map #(.getTags %) exif-directories)]
    (into {} (map extract-from-tag tags))))

(mapv (exif-for-file (io/file "resources/day12/pokemon/pikachu.jpg"))
 ["GPS Latitude"])

(let [metadata (ImageMetadataReader/readMetadata (java.io.File. "resources/day12/pokemon/pikachu.jpg"))
      exif-directories (.getDirectories metadata)]
  (doseq [x exif-directories] (println x)))

(defn parse-gps [gps ref]
  (let [dir (case ref 
              "N" 1
              "E" 1
              "S" -1
              "W" -1)
        [deg min sec] (->> (re-seq #"[0-9.]+" gps)
                           (map #(Double/parseDouble %)))]
    (* dir (+ deg (/ min 60) (/ sec 3600)))))

(defn parse-gps-coord [[lat lat-ref lon lon-ref]]
  [(parse-gps lat lat-ref)
   (parse-gps lon lon-ref)])

(defn bounding-box [[min-x min-y max-x max-y] [y x]]
  [(min min-x x)
   (min min-y y)
   (max max-x x)
   (max max-y y)])

(->> (file-seq (io/file "resources/day12/pokemon"))
     (filter #(.isFile %))
     (map exif-for-file)
     (map #(mapv % ["GPS Latitude" "GPS Latitude Ref" "GPS Longitude" "GPS Longitude Ref"]))
     (map parse-gps-coord)
     (map (partial apply format "x,%f,%f"))
     ;(take 10)
     (map println)
     doall
     )
