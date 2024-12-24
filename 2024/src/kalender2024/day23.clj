(ns kalender2024.day23
  (:import
   [java.awt Color]
   [java.awt.color ColorSpace]
   [java.awt.image BufferedImage]
   [java.io File]
   [javax.imageio ImageIO]))

(defn print-png [data w filename]
  (let [h (quot (count data) w)
        bi (BufferedImage. w h BufferedImage/TYPE_INT_RGB)]
    (.setRGB bi 0 0 w h data 0 w)
    (ImageIO/write bi "png" (File. (format "%s.png" filename)))))

(def data (slurp "https://julekalender-backend.knowit.no/challenges/2024-23/files/lekescan.txt?disposition=inline"))

(defn parse [in]
  (->> (re-seq #"[0-9.]+" in)
       (map #(Double/parseFloat %))
       (partition 3)))

(def points (vec (parse data)))

;; rgb
(defn points-to-array-as-rgb [points]
  (let [imagearray (int-array (count points))]
    (doseq [i (range (count points))]
      (let [[r g b] (points i)
            a (bit-shift-left 255 24)
            r (bit-shift-left (int (* r 255)) 16)
            g (bit-shift-left (int (* g 255)) 8)
            b (int (* b 255))]
        (aset-int imagearray i (unchecked-int (bit-or a r g b)))))
    imagearray))

;; hsv
(defn points-to-array-as-hsv [points]
  (let [imagearray (int-array (count points))]
    (doseq [i (range (count points))]
      (let [[h s v] (points i)
            a (bit-shift-left 255 24)]
        (aset-int imagearray i (unchecked-int (bit-or a (Color/HSBtoRGB h s v))))))
    imagearray))

;; CIEXYZ
(defn points-to-array-as-ciexyz [points]
  (let [imagearray (int-array (count points))
        colorspace (ColorSpace/getInstance ColorSpace/CS_CIEXYZ)]
    (doseq [i (range (count points))]
      (let [ciexyz (float-array (points i))
            rgb (.toRGB colorspace ciexyz)
            [r g b] (vec rgb)
            a (bit-shift-left 255 24)
            r (bit-shift-left (int (* r 255)) 16)
            g (bit-shift-left (int (* g 255)) 8)
            b (int (* b 255))]
        (aset-int imagearray i (unchecked-int (bit-or a r g b)))))
    imagearray))

(defn points-to-array-as-vectors [points dim]
  (let [imagearray (int-array (* dim dim))]
    (doseq [i (range (count imagearray))] (aset-int imagearray i (unchecked-int 0xff000000)))
    (doseq [[x y z] points]
      (let [x (int (* x (dec dim)))
            y (int (* y (dec dim)))]
        (when (< x 10) (println x y z))
        (aset-int imagearray (+ x (* y dim)) -1)))
    imagearray))

(defn data-as-circles-png [data w h filename]
  (let [bi (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)
        g (.createGraphics bi)]
    (.setColor g Color/BLACK)
    (.fillRect g 0 0 w h)
    (.setColor g Color/WHITE)
    (doseq [r data]
      (let [[x y z] r]
        (.fillOval g (* w y) (* h x) (* 3 z) (* 3 z))))
    (ImageIO/write bi "png" (File. (format "%s.png" filename)))))

(defn imagearray-to-png [imagearray]
  (let [c (count imagearray)
        possible-widths (filter #(= 0 (mod c %)) (range 10 600))]
    (doseq [w possible-widths]
      (print-png imagearray w (format "w-%03d" w)))))

(imagearray-to-png (points-to-array-as-rgb points))

(print-png (points-to-array-as-vectors points 1001) 1001 "vectors")

(data-as-circles-png points 500 500 "circles")