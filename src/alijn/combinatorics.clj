(ns alijn.combinatorics
  (:use [clj-todo.todo]
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
  [reference-groups target-groups]
  (let [blergh (->> (map all-pairs reference-groups target-groups)
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


;;; Sandbox
(todo
 "A big sandbox of ideas for refactoring..."
(comment

(defn smallest-combination
  "Finds the smallest combination of items from the sets,combined using the combiner.
Smallness is measured using metric."
  [combiner metric sets]
  (->> sets
       (apply cartesian-product)
       (map combiner)
       (apply min-key metric)))

(println 
 (smallest-combination 
  (fn [numbers] {:sum (apply + numbers) :numbers numbers})
  :sum 
  [[1 2] [3 4]]))

(defn smallest-pairing
  "Find the smallest pairing of all items from the sets, pairred using pair-up.
Smallnes is measured using metric."
  [pair-up metric sets]
  :not-implemented-but-could-probably-be-done-using-smallest-combination)

(println
 (smallest-pairing
  (fn [pairs-of-numbers]
    {:total-sum (reduce + (apply map + pairs-of-numbers)) 
     :pairs-of-numbers pairs-of-numbers})
  :total-sum
  [[1 2 3] [4 5] [5 6 7]]))
; => {:total-sum 27 :pairs-of-numbers [[2 3] [4 5] [6 7]]}
)
)