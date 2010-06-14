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

(deftest test-group-by
  (is (= {:foo [{:key :foo :elm 1} {:key :foo :elm 2}] :bar [{:key :bar :elm 3}]}
	 (group-by :key [{:key :foo :elm 1}
			 {:key :bar :elm 3}
			 {:key :foo :elm 2}])))
  (is (= {true [2 4 6] false [1 3 5]}
	 (group-by even? [1 2 3 4 5 6]))))

(deftest test-map-on-values
  (is (= {:foo 3 :bar 4 :baz 42}
	 (map-on-values inc {:foo 2 :bar 3 :baz 41}))))
