(ns alijn.molecule-alignment-test
  (:use [alijn.molecule-alignment] :reload-all)
  (:use [clojure.test]
	[clj-todo.todo])
  (:import [javax.vecmath Point3d]))

(def p1 (Point3d. 1 2 3))
(def p2 (Point3d. 0 1 2))
(def p3 (Point3d. 1 1 1))
(def q1 (Point3d. -1 0 0))
(def q2 (Point3d. -2 1 2))
(def q3 (Point3d. -4 -5 -6))

(def example-all-conformations
     {"conformations-A" [{"foo" [p1 p2], "bar" [q1 q2]} {"foo" [p1 p3], "bar" [q1 q2]}],
      "conformations-B" [{"foo" [p3 p2], "bar" [q3 q1]} {"foo" [p3 p1], "bar" [q3 q2]}]})

(deftest test-all-alignments-over-all-conformations
  (is (= :unreachable
	 (all-alignments-over-all-conformations example-all-conformations))))


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


;;;; Old rubbish tests

(comment deftest test-kabsch-on-pairing
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

(comment deftest test-optimal-pharmacophore-alignment
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
