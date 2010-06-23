(ns alijn.point-alignment
  (:use [clj-todo.todo]
	[alijn combinatorics kabsch])
  (:import [javax.vecmath Point3d]))

;;; Shiny new code (some untested)

(defn partition-using-sizes 
  "Partitions coll using the sizes from sizes."
  [sizes coll]
  (do
    (assert (= (count coll) (reduce + sizes)))
    (cond
     (seq sizes) 
      (cons 
       (take (first sizes) coll) 
       (partition-using-sizes (rest sizes) (drop (first sizes) coll)))
     :else nil)))

(defn flatten-groups
  "Flattens the groups into one long seq. Returns the flatted sequence
along with a function to unflatten a seq of same size as the flattened
back to the original structure."
  [groups]
  (let [flattened-groups (apply concat groups)
	sizes (map count groups)
	unflattener (partial partition-using-sizes sizes)]
    [flattened-groups unflattener]))

(def 
 all-alignments-on-labelled-pairings
 (memoize
  (fn
;    "Aligns the labelled points in reference with the labelled points in 
;labelled-points using all legal ways of pairing points from the two.
;The labelled points are maps where the keys are the labels and the
;values are the collections of Point3d's."
    [reference labelled-points]
    (let [labels (keys reference)
	  grouped-points (all-grouped-pairs (map reference labels) (map labelled-points labels))
	  flat-grouped-labels (map (partial map flatten-groups) grouped-points)]
      (map
       (fn [[[flat-reference _] [flat-labelled-points unflatten]]]
	 (if (and (> (count flat-reference) 0)
		  (> (count flat-labelled-points) 0))
	   (let [result (kabsch-with-translation flat-reference flat-labelled-points)]
	     (assoc result 
	       :rotated-points (->> result :rotated-points unflatten (zipmap labels))))
	   {:rmsd (Double/POSITIVE_INFINITY)
	    :no-solution true})) ; Pretty sure this is the culprit!
       flat-grouped-labels)))))

; Terrible names!
(def alignments-on-groups-pair all-alignments-on-labelled-pairings)

(defn optimal-alignment-on-all
  [reference-groups target-groups-groups]
  (map
   (fn [target-groups]
     (apply min-key :rmsd (alignments-on-groups-pair reference-groups target-groups)))
   target-groups-groups))

;all-alignments-on-conformation-pairings
(defn alignments-over-all-groups
  [group-of-groups]
  (let [elm (keys group-of-groups)]
    (map
     (fn [[reference-group-name target-groups-names]]
       (let [reference-group (group-of-groups reference-group-name)
	     target-groups (map group-of-groups target-groups-names)
	     alignments (optimal-alignment-on-all reference-group target-groups)
	     named-alignments (zipmap target-groups-names alignments)]
	 {:reference-name reference-group-name,
	  :reference-features reference-group,
	  :alignment named-alignments}))
     (leave-one-out elm))))

(defn alignment-rmsd-sum
  [alignment]
  (let [rmsds (map :rmsd (:alignment alignment))] 
    (apply reduce + rmsds)))

(defn smallest-alignment-rmsd-sum
  [alignments]
  (apply min-key alignment-rmsd-sum alignments))

; Terrible name
(defn optimal-alignment-over-all-groups
  [group-of-groups]
  (smallest-alignment-rmsd-sum
   (alignments-over-all-groups group-of-groups)))

;;;; Testing by printing :-s
(todo
 "All of this should become a unit test"
(comment
 
(def p1 (Point3d. 0 0 0))
(def p2 (Point3d. 1 1 1))
(def p3 (Point3d. 1 2 3))
(def q1 (Point3d. -1 -1 -1))
(def q2 (Point3d. 3 4 5))
(def q3 (Point3d. -1 1 -1))
(def u1 (Point3d. 3 2 1))
(def u2 (Point3d. 1 2 3))

(println 
 (alignments-on-groups-pair
  {"foo" [p1 p2], "bar" [p3]   }
  {"bar" [q1]   , "foo" [q2 q3]}))


(println)
(println 
 (select-optimal
  (alignments-on-groups-pair
   {"foo" [p1 p2], "bar" [p3]   }
   {"bar" [q1]   , "foo" [q2 q3]})))

(println "Little bang test")
(println 
 (optimal-alignment-on-all
  {"foo" [p1 p2], "bar" [p3]   }
  [
   {"bar" [q1]   , "foo" [q2 q3]}
   {"foo" [u1]   , "bar" [u2]   }
   ]))


(println "Big bang test")
(println (alignments-over-all-groups
	  {"A" {"foo" [p1 p2], "bar" [p3]   },
	   "B" {"bar" [q1]   , "foo" [q2 q3]},
	   "C" {"foo" [u1]   , "bar" [u2]   }}))

(println)
(println "Finding the best")
(println (optimal-alignment-over-all-groups 
	  {"A" {"foo" [p1 p2], "bar" [p3]   },
	   "B" {"bar" [q1]   , "foo" [q2 q3]},
	   "C" {"foo" [u1]   , "bar" [u2]   }}))


)
)

