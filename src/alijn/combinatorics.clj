(ns alijn.combinatorics
  (:use [clj-todo]
	[clojure.contrib.combinatorics]))

(defn shorter-permutations 
  "Lazy seq of all permutations of elements in items with
   only n items in returned sequences."
  [items n]
  (let [n (min n (count items))]
    (apply 
     concat 
     (map permutations 
	  (combinations items n)))))

(todo 
 "Should this be pushed to clojure.contrib? 
Might start with submitting it to mailing list"

(defn all-pairs 
  "Lazy seq of all pairings of elements from the two sequences."
  [seq-1 seq-2]
  (let [n (count seq-1) m (count seq-2)]
    (if (> n m)
      (map (partial map vector) 
	   (shorter-permutations seq-1 m)
	   (repeat seq-2))
      (map (partial map vector) 
	   (repeat seq-1)
	   (shorter-permutations seq-2 n)))))
)

(defn all-grouped-pairs
  "See alijn.combinatorics-test for an example usage."
  [colls-1 colls-2]
  (let [blergh (->> (map all-pairs colls-1 colls-2)
		    (apply cartesian-product))]
    (map 
     vector
     (map (partial map (partial map first)) blergh)
     (map (partial map (partial map second)) blergh))))
  
(defn leave-one-out
  "Generates a sequence of [elm rest] where elm is each element
of coll and rest are the remaining elements."
  [coll]
  (map
   (fn [n]
     [(nth coll n) 
      (concat (take n coll) (drop (inc n) coll))])
   (range (count coll))))
