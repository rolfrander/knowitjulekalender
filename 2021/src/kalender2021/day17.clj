(ns kalender2021.day17
  (:require [clojure.string :as string]))

(def testdata (string/split-lines "Arden Ånestad
Celinda Nysted
Helga
Helga Fossland
Helgalv
Ragnhild Rørvik
Sverre Aae
Emilee Trengereid
Elin Tveter
Maxima Viste
Åste Hansen"))

(def testdata2 (string/split-lines "Arden Ånestad
Celinda Nysted
Helga
Helga Fossland
Helga FOssland
Helga AAs
Helga Ås
Helga
Helgalv
Ragnhild Rørvik
Sverre Åe
Sverre AAe
Emilee Trengereid
Elin Tveter
Maxima Viste
Åste Hansen"))

(def data (string/split-lines (slurp "https://julekalender-backend.knowit.no/rails/active_storage/blobs/redirect/eyJfcmFpbHMiOnsibWVzc2FnZSI6IkJBaHBBc2NDIiwiZXhwIjpudWxsLCJwdXIiOiJibG9iX2lkIn19--668171c35d5406b640886290fcd386d895ebea51/alverekke.txt?disposition=inline")))

(def no_NO_x (java.text.RuleBasedCollator.  
              (str "< ' ' < A, a < B, b < C, c < D, d < E, e < F, f < G, "
                   "g < H, h < I, i < J, j < K, k < L, l < M, m < N, n < O, "
                   "o < P, p < Q, q < R, r < S, s < T, t < U, u < V, v < W, "
                   "w < X, x < Y, y < Z, z < "
                   "\u00C6, \u00E6 < " ; æ
                   "\u00D8, \u00F8 < " ; ø
                   "\u00C5 = A\u030A, \u00E5 = a\u030A"))) ; å, enten som separat tegn. eller som a-med-ring

(def alphabet " AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZzÆæØøÅå")
(def ord (into {} (map-indexed #(vector %2 %1) alphabet)))

(defn largest2 [^String a ^String b]
  (letfn [(ord-idx [^String str idx]
            (if (>= idx (.length str)) -1
                (ord (.charAt str idx))))]
    (loop [i 0]
      (let [la (ord-idx a i)
            lb (ord-idx b i)]
        (cond
          (= la lb) (recur (inc i))
          (> la lb) a
          :else b)))))

(defn largest [a b] 
  (if (> (.compare no_NO_x a b) 0) a b))

(defn test [compare-fn d]
  (->> (reductions compare-fn d)
       dedupe
       (map count)
       (apply +)))

(time (test largest data))
(time (test largest2 data))