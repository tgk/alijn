(ns alijn.molecule-alignment
  (:use clojure.contrib.combinatorics)
  (:use clj-todo.todo)
  (:use [alijn.pharmacophore]
	[alijn.kabsch]))

; Pairing of elements

(todo
 "Should this be pushed to clojure.contrib?"

(defn shorter-permutations 
  "Lazy seq of all permutations of elements in items with
   only n items in returned sequences.
   Assumes n is smaller or equal to the number of items."
  [items n]
  (let [n (min n (count items))]
    (apply 
     concat 
     (map permutations 
	  (combinations items n)))))
)

(todo 
 "Should this be pushed to clojure.contrib? 
Might start with submitting it to mailing list"

(defn all-pairs 
  "Lazy seq of all pairings of elements from the two sequences."
  [seq-1 seq-2]
  (let [n (count seq-1) m (count seq-2)]
    (if (> n m)
      ; There is redundant code here. Can it be solved somehow?
      (map (partial map vector) 
        (shorter-permutations seq-1 m)
        (repeat seq-2))
      (map (partial map vector) 
        (repeat seq-1)
        (shorter-permutations seq-2 n)))))
)

