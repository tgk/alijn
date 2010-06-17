(ns alijn.point-alignment-test
  (:use [alijn.point-alignment] :reload-all)
  (:use [clojure.test]
	[clj-todo.todo])
  (:import [javax.vecmath Point3d]))

(deftest test-partition-using-sizes
  (is (= [[:foo] [:bar :baz]] 
	 (partition-using-sizes [1 2] [:foo :bar :baz])))
  (is (= [[:foo :bar] [:baz :foobar]] 
	 (partition-using-sizes [2 2] [:foo :bar :baz :foobar])))
  (is (= [[:foo] [] [:bar] [:baz]] 
	 (partition-using-sizes [1 0 1 1] [:foo :bar :baz])))
  (is (= [[]] 
	 (partition-using-sizes [0] []))))

(deftest test-flatten-groups
  (let [groups [[:foo :bar] [:baz]]
	[flat unflatten-groups] (flatten-groups groups)]
    (is (= [:foo :bar :baz] flat))
    (is (= groups (unflatten-groups flat))))
  (let [groups [[]]
	[flat unflatten-groups] (flatten-groups groups)]
    (is (= [] flat))
    (is (= groups (unflatten-groups flat))))
  (let [groups [[:foo] [] [:bar :baz]]
	[flat unflatten-groups] (flatten-groups groups)]
    (is (= [:foo :bar :baz] flat))
    (is (= groups (unflatten-groups flat)))))

(deftest test-vec-center
  (is (= (Point3d. 1 0 2)
	 (vec-center [(Point3d. 0 -10 0)
		      (Point3d. 2  10 4)]))))

(deftest test-center-points
  (is (= [(Point3d. -5 0 0) (Point3d. 5 0 0)]
	 (center-points [(Point3d. 10 0 0)
			 (Point3d. 20 0 0)]))))
	 

(def p1 (Point3d. 1 2 3))
(def p2 (Point3d. 0 1 2))
(def p3 (Point3d. 1 1 1))
(def q1 (Point3d. -1 0 0))
(def q2 (Point3d. -2 1 2))
(def q3 (Point3d. -4 -5 -6))

(deftest test-optimal-alignment-over-all-groups
  (is (= :unreachable
	 (optimal-alignment-over-all-groups
	  {"conformations-A" {"foo" [p1 p2], "bar" [q1 q2]},
	   "conformations-B" {"foo" [p3 p2], "bar" [q3 q1]}}))))

