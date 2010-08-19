(ns alijn.colored-point-alignment-test
  (:use [alijn.colored-point-alignment] :reload-all
	[clojure.test]
	clj-todo)
  (:import [javax.vecmath Point3d]))

(defn within-epsilon [epsilon a b]
  (< (Math/abs (- a b)) epsilon))

(defn rmsd= [oracle-rmsd result]
  (within-epsilon 10E-5 oracle-rmsd (:rmsd result)))

(deftest test-two-points-two-dimensions-exhaustive
  (is (rmsd= (Math/sqrt 1)
	 (exhaustive-point-alignment 
	  [[(Point3d.  0  2  0) (Point3d.  0 -2  0)]]
	  [[(Point3d.  1  0  0) (Point3d. -1  0  0)]])))
  (is (rmsd= (Math/sqrt 1)
	 (exhaustive-point-alignment 
	  [[(Point3d.  0  2  0) (Point3d.  0 -2  0)]]
	  [[(Point3d. 10  2  0) (Point3d. 10  0  0)]])))
  (is (rmsd= (Math/sqrt 1)
	 (exhaustive-point-alignment 
	  [[(Point3d.  0  2  0) (Point3d.  0 -2  0)]]
	  [[(Point3d. 10  2  0) (Point3d. 12  2  0)]])))
  (is (rmsd= (Math/sqrt 1)
	 (exhaustive-point-alignment 
	  [[(Point3d. -5  2  0) (Point3d. -5 -2  0)]]
	  [[(Point3d.  1  0  0) (Point3d. -1  0  0)]])))
  (is (rmsd= (Math/sqrt 1)
	 (exhaustive-point-alignment 
	  [[(Point3d. -5  2  0) (Point3d. -5 -2  0)]]
	  [[(Point3d. 10 -3  0) (Point3d. 12 -3  0)]]))))

(deftest test-one-has-two-points-other-has-four-exhaustive
  (is (rmsd= 0
	 (exhaustive-point-alignment 
	  [[(Point3d. -2  0  0) (Point3d.  2  0  0)]]
	  [[(Point3d. -2  0  0) (Point3d.  0  2  0) 
	    (Point3d.  2  0  0) (Point3d.  0 -2  0)]])))
  (is (rmsd= 0
	 (exhaustive-point-alignment 
	  [[(Point3d. -2  0  0) (Point3d.  0  2  0) 
	    (Point3d.  2  0  0) (Point3d.  0 -2  0)]]
	  [[(Point3d. -2  0  0) (Point3d.  2  0  0)]]))))

(deftest test-two-colors-one-point-of-each-color-exhaustive
  (is (rmsd= (Math/sqrt 1)
	 (exhaustive-point-alignment
	  [[(Point3d. -2  0  0)] [(Point3d.  2  0  0)]]
	  [[(Point3d.  0  1  0)] [(Point3d.  0 -1  0)]])))
  (is (rmsd= 0
	 (exhaustive-point-alignment
	  [[(Point3d. -1  0  0)] [(Point3d.  1  0  0)]]
	  [[(Point3d.  0  1  0)] [(Point3d.  0 -1  0)]]))))

(deftest test-two-colors-two-points-each-exhaustive
  (let [r  (Math/sqrt 2)
	-r (- r)]
    (is (rmsd= 0
	   (exhaustive-point-alignment
	    [[(Point3d. -2  0  0) (Point3d.  0  2  0)]
	     [(Point3d.  2  0  0) (Point3d.  0 -2  0)]]
	    [[(Point3d. -r  r  0) (Point3d.  r  r  0)]
	     [(Point3d. -r -r  0) (Point3d.  r -r  0)]])))))

(deftest test-3d-exhaustive
  (is (rmsd= (Math/sqrt 3)
	 (exhaustive-point-alignment
	  [[(Point3d.  1  1  1)] [(Point3d. -1 -1 -1)]]
	  [[(Point3d. -2 -2 -2)] [(Point3d.  2  2  2)]])))
  (is (rmsd= (Math/sqrt 3)
	 (exhaustive-point-alignment
	  [[(Point3d.  3  3  3)] [(Point3d.  1  1  1)]]
	  [[(Point3d. -2 -2 -2)] [(Point3d.  2  2  2)]]))))

(deftest test-moved-points
  (is (=
       [[(Point3d. -8 10  0) (Point3d. -6 10  0)]]
       (:moved-variable
	(exhaustive-point-alignment
	 [[(Point3d. -9 10  0) (Point3d. -5 10  0)]]
	 [[(Point3d.  5  2  0) (Point3d.  7  2  0)]])))))