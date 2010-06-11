(ns alijn.molecule-alignment-test
  (:use [alijn.molecule-alignment] :reload-all)
  (:use [clojure.test]
	[clj-todo.todo])
  (:import [javax.vecmath Point3d]))


(todo
"More thorough testing needed."

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
)
)

(todo
"More testing needed."

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
				  (Point3d. -1  0  0)]}]))))
)