(ns alijn.molecule-alignment-test
  (:use [alijn.molecule-alignment] :reload-all)
  (:use [clojure.test]
	[clj-todo.todo])
  (:import [javax.vecmath Point3d]))

(deftest test-vec-center
  (is (= (Point3d. 1 0 2)
	 (vec-center [(Point3d. 0 -10 0)
		      (Point3d. 2  10 4)]))))

(deftest test-center-points
  (is (= [(Point3d. -5 0 0) (Point3d. 5 0 0)]
	 (center-points [(Point3d. 10 0 0)
			 (Point3d. 20 0 0)]))))
	 
(deftest test-kabsch-on-pairing
  (is (= {:rmsd 0.0
	  :result [["foo" (Point3d.  1  0  0)] 
		   ["foo" (Point3d. -1  0  0)]
		   ["bar" (Point3d.  0  1  0)]
		   ["bar" (Point3d.  0 -1  0)]]}
	 (kabsch-on-pairing [["foo" [(Point3d.  1  0  0) (Point3d.  0  1  0)]]
			     ["foo" [(Point3d. -1  0  0) (Point3d.  0 -1  0)]]
			     ["bar" [(Point3d.  0  1  0) (Point3d.  1  0  0)]]
			     ["bar" [(Point3d.  0 -1  0) (Point3d. -1  0  0)]]])))
  (is (= {:rmsd 0.0
	  :result [["foo" (Point3d.  1  0  0)] 
		   ["foo" (Point3d. -1  0  0)]
		   ["bar" (Point3d.  0  1  0)]
		   ["bar" (Point3d.  0 -1  0)]]}
	 (kabsch-on-pairing [["foo" [(Point3d.  2  0  0) (Point3d.  0  1  0)]]
			     ["foo" [(Point3d.  0  0  0) (Point3d.  0 -1  0)]]
			     ["bar" [(Point3d.  1  1  0) (Point3d.  1  0  0)]]
			     ["bar" [(Point3d.  1 -1  0) (Point3d. -1  0  0)]]]))))

(todo
"More testing needed. Both on something that gets translated, but also on data
that has no pairings."

(deftest test-optimal-pharmacophore-alignment
  (is (= {:rmsd 0.0
	  :result [["foo" (Point3d.  1  0  0)] 
		   ["foo" (Point3d. -1  0  0)]
		   ["bar" (Point3d.  0  1  0)]
		   ["bar" (Point3d.  0 -1  0)]]}
	 (optimal-pharmacophore-alignment
	  [{:name "foo" :centers [(Point3d.  1  0  0) 
				  (Point3d.  2  0  0)
				  (Point3d. -1  0  0) 
				  (Point3d. -2  0  0)]}
	   {:name "bar" :centers [(Point3d.  0  1  0)
				  (Point3d.  0 -1  0)]}]
	  [{:name "foo" :centers [(Point3d.  0 -1  0)
				  (Point3d.  0  1  0)]}
	   {:name "bar" :centers [(Point3d.  1  0  0) 
				  (Point3d. -1  0  0)]}])))
  (is (= {:rmsd 0.0
	  :result [["foo" (Point3d.  1  0  0)] 
		   ["foo" (Point3d. -1  0  0)]
		   ["bar" (Point3d.  0  1  0)]
		   ["bar" (Point3d.  0 -1  0)]]}
	 (optimal-pharmacophore-alignment
	  [{:name "foo" :centers [(Point3d.  1  0  0) 
				  (Point3d.  2  0  0)
				  (Point3d. -1  0  0) 
				  (Point3d. -2  0  0)]}
	   {:name "bar" :centers [(Point3d.  0  1  0)
				  (Point3d.  0 -1  0)]}
	   {:name "baz" :centers [(Point3d. 42 42 42)]}]
	  [{:name "foo" :centers [(Point3d.  0 -1  0)
				  (Point3d.  0  1  0)]}
	   {:name "bar" :centers [(Point3d.  1  0  0) 
				  (Point3d. -1  0  0)]}
	   {:name "baz" :centers []}]))))
)