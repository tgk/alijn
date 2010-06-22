(ns alijn.math-test
  (:use [alijn.math] :reload-all)
  (:use [clojure.test])
  (:import [javax.vecmath Point3d]
	   [Jama Matrix]))

(deftest test-vec-center
  (is (= (Point3d. 1 0 2)
	 (vec-center [(Point3d. 0 -10 0)
		      (Point3d. 2  10 4)]))))

(deftest test-matrix-vector-product
  (let [matrix (Matrix. 3 3)
	vector (Point3d. 1 2 3)]
    (.set matrix 0 0 1)
    (.set matrix 1 1 1)
    (.set matrix 2 2 1)
    (is (= vector
	   (matrix-vector-product matrix vector))))
  (let [matrix (Matrix. 3 3)
	vector (Point3d. 1 2 3)]
    (.set matrix 1 0 4)
    (.set matrix 1 1 2)
    (.set matrix 1 2 3)
    (is (= (Point3d. 0 17 0)
	   (matrix-vector-product matrix vector)))))
