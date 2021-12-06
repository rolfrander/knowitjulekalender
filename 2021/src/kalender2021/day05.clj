(ns kalender2021.day05)

(def testdata "Aurora(Toralv(Grinch(Kari Robinalv) Alvborg) Grinch(Alva(Alve-Berit Anna) Grete(Ola Hans)))")

(re-seq #"[A-Z][a-z]+|\(|\)" testdata)

; X = token "(" X X ")"
; X = token


(defn parse [input]
  (let [matcher (re-matcher #"[A-Za-zÆØÅæøå-]+|\(|\)| " input)

        token (atom (re-find matcher))
        
        pos (atom 0)]
    
    (letfn [
        (tok [] (let [t (re-find matcher)]
                  (when t
                    (swap! pos #(+ (.length t) %))
                    (if (= t " ")
                      (tok)
                      (reset! token t)))))

        (match-name []
                    (let [name @token]
                      (if (Character/isAlphabetic (int (.charAt name 0)))
                        (do (tok) name)
                        nil)))

        (match-left-par [] (if (= @token "(")
                                (do (tok) true)
                                nil))

        (match-right-par [] (if (= @token ")")
                                 (do (tok) true)
                                 nil))

        (match-tree [depth]
                    (if-let [name (match-name)]
                      (let [count (if (= name "Grinch") 0 1)]
                        (if (match-left-par)
                          (let [left (match-tree (+ count depth))
                                right (match-tree (+ count depth))]
                            (if (match-right-par)
                              (max left right)
                              (throw (RuntimeException. (str "expected ')' got '" @token "' at " @pos)))))
                          depth))
                      (throw (RuntimeException. (str "expected name got '" @token "' at " @pos)))))]

    (match-tree 0))))


(let [m (re-matcher #"[A-Z][a-z]+|\(|\)" testdata)]
  (match-tree (re-find m) m))

(Character/isAlphabetic (int (.charAt "Foo" 0)))

(parse testdata)
(parse (slurp "https://julekalender-backend.knowit.no/rails/active_storage/blobs/redirect/eyJfcmFpbHMiOnsibWVzc2FnZSI6IkJBaHBBak1DIiwiZXhwIjpudWxsLCJwdXIiOiJibG9iX2lkIn19--d6d3984e0f603df1771ef6b699e6e86d6ee577a7/tree.txt"))