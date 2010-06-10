(ns alijn.molecule-alignment
  (:use clojure.contrib.combinatorics)
  (:use clj-todo.todo)
  (:use [alijn.pharmacophore]
	[alijn.kabsch]))

; Pairing of elements
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

(defn grouped-all-pairs 
  "Generates all grouped pairs as in
[[:f1 :f2] [:b1]] [[:f3] [:b2 :b3]] => 
[[[:f1 :f3] [:b1 :b2]] [[:f1 :f3] [:b1 :b3]] [[:f2 :f3] [:b1 :b2]] [[:f2 :f3] [:b1 :b3]]]"
  [target-groups subject-groups]
  (->> (map all-pairs target-groups subject-groups)
       (apply cartesian-product)
       (map (partial apply concat))))

; Pairing of pharmacopohores
(defn pharmacophore-pairings
  "Returns all point pairings from two seqs of {:name :centers}.
Assumes the names arrives in same order in both seqeunces.
Result is (([target-point subject-point] [t-point s-point] ...) ...)."
  [target subject]
  (grouped-all-pairs (map :centers target) (map :centers subject)))