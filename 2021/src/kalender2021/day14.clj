(ns kalender2021.day14
  (:require [clojure.string :as string]))

(def data (string/split-lines (slurp "https://julekalender-backend.knowit.no/rails/active_storage/blobs/redirect/eyJfcmFpbHMiOnsibWVzc2FnZSI6IkJBaHBBcnNDIiwiZXhwIjpudWxsLCJwdXIiOiJibG9iX2lkIn19--6831d5c3e2a2dd4afaf03b75859fc803cafcef20/ordliste.txt?disposition=inline")))

(take 10 data)

(defn word-check-function [^String find-word ^long min-space ^long max-space]
  (let [minimum-space-needed (+ (.length find-word)
                                (* (dec (.length find-word)) min-space))]
    (fn [^String line]
      (let [line-len (.length line)]
        (loop [pos 0
               word (seq find-word)
               space 0
               stack '(-1)]
          (cond
            (> pos 100) (throw (RuntimeException. "out of bounds..."))
            
            (empty? stack)
            false

            ; moved to far ahead => track back to start and try from next letter
            (> space max-space)
            (recur (peek stack) (seq find-word) 0 (pop stack))

            ; no more letters to match, ok, we are done
            (empty? word)
            true

            ; past end of line, maybe restart?
            (>= pos line-len)
            (if (and (>= (peek stack) 0) (> (- line-len (peek stack)) (count word)))
                (recur (inc (peek stack)) (seq word) 0 (pop stack))
                false)

            ; match letter, jump min-space ahead and continue looking for the rest of word
            (= (.charAt line pos) (first word))
            (recur (+ pos min-space 1) (rest word) min-space (conj stack pos))
            
            ; no match, step forward, count spacing if we are past first letter
            :else
            (recur (inc pos) word (if (>= (peek stack) 0) (inc space) 0) stack)))))))

(let [t? (word-check-function "troll" 1 5)
      n? (word-check-function "nisse" 0 2)
      test (fn [func param expected]
             (when (not= (func param) expected)
               (println "failed:" param " expected:" expected)))]
  (test t? " t r o ll" false)
  (test t? "t r o l l" true)
  (test t? "t     r     o l l" true)
  (test t? " ttrroollll " true)
  (test t? "troll" false)
  (test n? "nisse" true)
  (test n? "nnnnisse" true)
  (test n? "nniissee" true)
  (test n? " nissniss s e " true)
  (test n? "acetontilsatsen" true)
  
  )

(let [troll? (word-check-function "troll" 1 5)
      nisse? (word-check-function "nisse" 0 2)
      troll-or-nisse? (fn [w] (or (and (not= (first w) \n)
                                       (not= (last w) \e)
                                       (nisse? w))
                                  (troll? w)))]
  ;(troll-or-nisse? "tnrioslsle")
  (count (filter troll-or-nisse? data)))
  

(defn xor [a b]
  (or (and a (not b))
      (and b (not a))))

(let [re-troll? #".*t.{1,5}r.{1,5}o.{1,5}l.{1,5}l.*"
      re-nisse? #"[^n].*n.{0,2}i.{0,2}s.{0,2}s.{0,2}e.*[^e]"
      re-troll-or-nisse? (fn [w] (or (re-matches re-nisse? w)
                                     (re-matches re-troll? w)))
      troll? (word-check-function "troll" 1 5)
      nisse? (word-check-function "nisse" 0 2)
      troll-or-nisse? (fn [w] (or (and (not= (first w) \n)
                                       (not= (last w) \e)
                                       (nisse? w))
                                  (troll? w)))

      either-or? (fn [w] (xor (re-troll-or-nisse? w)
                              (troll-or-nisse? w)))]
  (filter either-or? (take 50000 data))
  )
  



