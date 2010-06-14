(ns alijn.combinatorics
  (:use [clj-todo.todo]
	[clojure.contrib.combinatorics]))

(todo
 "I moved pairing of pharmacophores away from it's own namespace 
to limit the number of namespaces, but now I can't figure out if that was overkill."
nil)
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
  "Generates all grouped pairs as in [[:f1 :f2] [:b1]] [[:f3] [:b2 :b3]] => 
  [[[:f1 :f3] [:b1 :b2]] [[:f1 :f3] [:b1 :b3]] [[:f2 :f3] [:b1 :b2]] [[:f2 :f3] [:b1 :b3]]]"
  [reference-groups subject-groups]
  (->> (map all-pairs reference-groups subject-groups)
       (apply cartesian-product)
       (map (partial apply concat))))

(todo 
 "Maybe move this along with other combinatoric stuff to seperat
combinatorics namespace.
Also: should this be a [& coll] to make it nicer?"

(defn leave-one-out
  "Generates a sequence of [elm rest] where elm is each element
of coll and rest are the remaining elements."
  [coll]
  (map
   (fn [n]
     [(nth coll n) 
      (concat (take n coll) (drop (inc n) coll))])
   (range (count coll))))
)
