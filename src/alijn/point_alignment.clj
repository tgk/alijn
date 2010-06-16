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

(defn vec-sub 
  "u - v, where u and v are Point3d vectors."
  [u v]
  (let [result (Point3d.)]
    (.sub result u v)
    result))

(defn vec-center 
  "Finds the vector center for a seq of Point3d."
  [points]
  (let [result (Point3d. 0 0 0)]
    (doseq [point points] (.add result point))
    (.scale result (/ 1 (count points)))
    result))
  
(defn center-points
  [points]
  (let [center (vec-center points)
	moved-points (map #(vec-sub %1 center) points)] 
    moved-points))

(todo 
"Might check that the group names are the same!
Also, this is a huge chunk of code, but I don't feel like splitting it up."
(defn alignments-on-groups-pair
  [reference-groups target-groups]
  (let [group-names (keys reference-groups)
	ref-groups (map (partial get reference-groups) group-names)
	tar-groups (map (partial get target-groups   ) group-names)
	pairs-of-flat-ref-and-target (map 
				      (partial map flatten-groups)
				      (all-grouped-pairs ref-groups tar-groups))]
    (map
     (fn [[[flat-ref-group _] [flat-target-group target-unflatten]]]
       (let [ref-points (center-points flat-ref-group)
	     tar-points (center-points flat-target-group)
	     result (kabsch ref-points tar-points)
	     rmsd (rmsd ref-points result)
	     unflat-result (target-unflatten result)
	     group-named-result (zipmap group-names unflat-result)]
	 {:result group-named-result
	  :rmsd   rmsd}))
     pairs-of-flat-ref-and-target)))
)

(todo
"Must be idomatic way not using an entire sorted-set, like min."

(defn select-optimal
  "Can be used for both alignment-on-group-pairs and optimal alignment over all groups."
  [results]
  (first
   (apply sorted-set-by
	  (fn [{rmsd-1 :rmsd} {rmsd-2 :rmsd}] (compare rmsd-1 rmsd-2))
	  results)))
)

(defn optimal-alignment-on-all
  [reference-groups target-groups-groups]
  (map
   (fn [target-groups]
     (select-optimal (alignments-on-groups-pair reference-groups target-groups)))
   target-groups-groups))

(defn alignments-over-all-groups
  "Input: {name-1 {group-name-1 [a1 a2], group-name-2 [b2]}, ... }
Output: {:reference name-k, :rmsds [42.367, ... ], 
         :alignment {name-1 {group-name-1 [p3], ... }, ... }}"
  [group-of-groups]
  (let [elm (keys group-of-groups)]
    (map
     (fn [[reference-group-name target-groups-names]]
       (let [reference-group (group-of-groups reference-group-name)
	     target-groups (map group-of-groups target-groups-names)
	     alignments (optimal-alignment-on-all reference-group target-groups)
	     named-alignments (zipmap target-groups-names (map :result alignments))
	     rmsds (map :rmsd alignments)]
	 {:reference reference-group-name,
	  :alignment named-alignments,
	  :rmsds rmsds}))
     (leave-one-out elm))))

(defn optimal-alignment-over-all-groups
  [group-of-groups]
  (select-optimal
   (map 
    #(assoc % :rmsd (reduce + (:rmsds %))) 
    (alignments-over-all-groups group-of-groups))))

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