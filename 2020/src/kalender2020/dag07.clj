(ns kalender2020.dag07
  (:require [clojure.string :as str]))

(def data (slurp "https://julekalender-backend.knowit.no/challenges/7/attachments/forest.txt"))

(def testdata "                                         
                          #              
                         ###             
                        #####            
                       #######           
      #               #########          
     ###                 ###             
    #####                #####           
   #######             #######           
  #########           #########          
 ###########             ###             
#############           #####            
     ###               #######           
    #####             #########          
   #######               ###             
  #########             #####            
 ###########           #######           
#############    #    #########    #     
     ###        ###      ###      ###    
    #####      ### #    #####    # # #   
   #######      ###    #######    ###    
  #########    #####  #########  #####   
 ###########    ###      ###      ###    
#############  #####    #####    #####   
      #          #        #        #     
      #          #        #        #     ")


(defn tree-offsets [data]
  (->> data
       peek
       (map-indexed #(if (= \# %2) %1 -1))
       (filter #(>= % 0))))

(defn symmetrical?
  "returns true if the tree at the given offset is symmetrical in this line"
  [^String line offset]
  (let [len (count line)
        iffset (int offset)]
    (loop [i 1
           hole false]
      (let [left (- offset i)
            leftchar (if (< left 0) \space (.charAt line left))
            right (+ offset i)
            rightchar (if (>= right len) \space (.charAt line right))]
        (if (and (= leftchar rightchar) 
                 (or (= \# leftchar)
                     (not hole))) ; can accept left=right=\space (a hole) unless the last iteration was a hole
          (recur (inc i) (= \space leftchar))
          (= leftchar rightchar))))))

(defn examine-tree-level 
  "line represents a cut through all trees at a certain level. Offset represents
   the offsets in this line where we expect to find tree-trunks. Returns the
   offsets having valid (symmetrical) trees in this line (at this level)"
  [offsets line]
  (filter #(symmetrical? line %) offsets)
  )

(time 
 (let [trees (str/split-lines data)
       off (tree-offsets trees)]
   (count (reduce examine-tree-level off trees))))

(tree-offsets (str/split-lines testdata))

(and 
;              0123456789
 (= (symmetrical? "     ####### " 9) false)
 (= (symmetrical? "     ####### " 8) true)
 (= (symmetrical? "     ####### " 7) false)
 (= (symmetrical? "     #######" 9) false)
 (= (symmetrical? "     #######" 8) true)
 (= (symmetrical? "     #######" 7) false)
 (= (symmetrical? "####### " 4) false)
 (= (symmetrical? "####### " 3) true)
 (= (symmetrical? "####### " 7) false))
;              0123456789
(symmetrical? "  # # #" 4)
(symmetrical? "  # # ##" 4)
(symmetrical? "#  ###  ##" 4)

;                    0         1         2         3         4         1         6
;                    0123456789012345678901234567890123456789012345678901234567890123456789
(examine-tree-level "     #      ###       ####    # # #    # ###" [5 13 23 32 41])
