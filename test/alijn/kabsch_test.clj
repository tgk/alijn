(ns alijn.kabsch-test
  (:use [alijn.kabsch] :reload-all
	[clojure.test]
	clj-todo)
  (:import [Jama Matrix]
	   [javax.vecmath Point3d]))

;;; Definition of test data
;;;; Tests with perfect optimal overlay
(def test-1-points-1 [(Point3d. 1 0 0)])
(def test-1-points-2 [(Point3d. 0 1 0)])

(def test-2-points-1 [(Point3d.  1 0 0) (Point3d.  2 0 0)])
(def test-2-points-2 [(Point3d. -1 0 0) (Point3d. -2 0 0)])

(def test-3-points-1 [(Point3d.  1  1  1)])
(def test-3-points-2 [(Point3d. -1 -1 -1)])

(def test-4-points-1 [(Point3d.  1  0 0)
		      (Point3d. -1  0 0)
		      (Point3d.  0  1 0)
		      (Point3d.  0 -1 0)])
(def s (Math/sqrt 0.5))
(def -s (- s))
(def test-4-points-2 [(Point3d.  s -s 0)
		      (Point3d. -s  s 0)
		      (Point3d.  s  s 0)
		      (Point3d. -s -s 0)])

;;;; Tests with non-perfect optimal overlay
(def test-5-points-1 [(Point3d. 0 0 0)])
(def test-5-points-2 [(Point3d. 0 1 0)])

;;;; Limit for how precise Kabsch has to be
(def epsilon 1.0E-6)

(deftest test-kabsch-on-rmsds-with-perfect-possible-rotation
  (is (> epsilon (:rmsd (kabsch test-1-points-1 test-1-points-2))))
  (is (> epsilon (:rmsd (kabsch test-2-points-1 test-2-points-2))))
  (is (> epsilon (:rmsd (kabsch test-3-points-1 test-3-points-2))))
  (is (> epsilon (:rmsd (kabsch test-4-points-1 test-4-points-2)))))

(deftest test-kabsch-on-rmsds-with-non-perfect-possible-rotation
  (is (= 1.0 (:rmsd (kabsch test-5-points-1 test-5-points-2)))))

(deftest test-kabsch-with-translation
  (is (= 1
	 (:rmsd
	  (kabsch-with-translation
	    [(Point3d. -5  2  0) (Point3d. -5 -2  0)]
	    [(Point3d.  1  0  0) (Point3d. -1  0  0)]))))
  (is (= 1
	 (:rmsd
	  (kabsch-with-translation
	    [(Point3d. -5  2  0) (Point3d. -5 -2  0)]
	    [(Point3d. 10 -3  0) (Point3d. 12 -3  0)])))))

