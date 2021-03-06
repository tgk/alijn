(ns alijn.colored-point-alignment-test
  (:use [alijn.colored-point-alignment] :reload-all
	[clojure.test]
	clojure.pprint)
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

(deftest test-empty-vectors-exhaustive
  (is (rmsd= (Math/sqrt 1)
	 (exhaustive-point-alignment 
	  [[(Point3d.  0  2  0) (Point3d.  0 -2  0)] []]
	  [[(Point3d.  1  0  0) (Point3d. -1  0  0)] []]))))

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

;;; Clique based algorithm

(deftest test-wrap-points
  (is (= [{:elm :a, :color 0} {:elm :b, :color 0}, {:elm :c, :color 1}]
	 (wrap-points [[:a :b] [:c]])))
  (is (= [] (wrap-points [])))
  (is (= [{:elm :a, :color 0} 
	  {:elm :b, :color 1} {:elm :c, :color 1} 
	  {:elm :d, :color 2}]
	 (wrap-points [[:a] [:b :c] [:d]]))))

(deftest test-unwrap-points
  (is (= [[:a :b] [:c]]
	 (unwrap-points 
	  [0 1]
	  [{:elm :a, :color 0} {:elm :b, :color 0} {:elm :c, :color 1}])))
  (is (= [] (unwrap-points [] [])))
  (is (= [[:a :b] [:c]]
	 (unwrap-points
	  [0 1]
	  [{:elm :a, :color 0} {:elm :c, :color 1} {:elm :b, :color 0}])))
  (is (= [[:a :b] [] [:c]]
	 (unwrap-points
	  [0 1 2]
	  [{:elm :a, :color 0} {:elm :c, :color 2} {:elm :b, :color 0}])))
  (is (= [[:a :b] [:c] []]
	 (unwrap-points
	  [0 1 2]
	  [{:elm :a, :color 0} {:elm :c, :color 1} {:elm :b, :color 0}]))))

(deftest test-clique-grouped-pairs
  (let [b1 (Point3d. 0 0 0)
	b2 (Point3d. 2 0 0)
	r1 (Point3d. 4 0 0)
	b3 (Point3d. 0 0 0)
	b4 (Point3d. 1 0 0)
	r2 (Point3d. 4 0 0)]
    (is (= [[[[b1] [r1]] [[b3] [r2]]]]
	   (clique-grouped-pairs 0 [[b1 b2] [r1]] [[b3 b4] [r2]])))))

(deftest test-clique-based-point-alignment
  (let [r1 (Point3d. -1  3  0)
	r2 (Point3d.  3  3  0)
	b1 (Point3d.  1  2  0)
	b2 (Point3d.  1  4  0)
	r3 (Point3d.  1  3  0)
	r4 (Point3d.  1  5  0)
	b3 (Point3d.  0  4  0)
	b4 (Point3d.  2  4  0)
	colored-points-1 [[r1 r2] [b1 b2]]
	colored-points-2 [[r3 r4] [b3 b4]]]
  (is (rmsd= 
       0
       (clique-based-point-alignment 0 colored-points-1 colored-points-2)))
  (is (rmsd= 
       (Math/sqrt (/ (+ 0 0 1 1) 4))
       (clique-based-point-alignment 2 colored-points-1 colored-points-2)))))

;;; Error discovered on flexs set

(deftest test-error-on-clique-grouped-pairs-from-flexs
  (let [feature-points {"negative"
			[(Point3d. -0.2793, -2.9642, -0.0718)
			 (Point3d. -2.3723, -2.9332, -0.1578)
			 (Point3d. 1.3747, -3.3622, -3.0318)
			 (Point3d. 1.7407, -1.3452, -2.0438)],
			"positive" [],
			"aromatic-rings"
			[(Point3d. -0.4481333333333335, 
				   -1.9382000000000006, 
				   -6.334633333333335)],
			"acceptor"
			[(Point3d. -0.2793, -2.9642, -0.0718)
			 (Point3d. -2.3723, -2.9332, -0.1578)
			 (Point3d. 1.3747, -3.3622, -3.0318)
			 (Point3d. 1.7407, -1.3452, -2.0438)],
			"donor" ()}
	feature-ks (keys feature-points)
	feature-points-lists (map feature-points feature-ks)
	grouped-pairs (clique-grouped-pairs 0.0 
					    feature-points-lists
					    feature-points-lists)]
    (is (= 1 (count grouped-pairs))))) ; 1 is the wrong answer!

(deftest test-error-on-clique-based-point-alignment-from-flexs
  (let [feature-points {"negative"
			[(Point3d. -0.2793, -2.9642, -0.0718)
			 (Point3d. -2.3723, -2.9332, -0.1578)
			 (Point3d. 1.3747, -3.3622, -3.0318)
			 (Point3d. 1.7407, -1.3452, -2.0438)],
			"positive" [],
			"aromatic-rings"
			[(Point3d. -0.4481333333333335, 
				   -1.9382000000000006, 
				   -6.334633333333335)],
			"acceptor"
			[(Point3d. -0.2793, -2.9642, -0.0718)
			 (Point3d. -2.3723, -2.9332, -0.1578)
			 (Point3d. 1.3747, -3.3622, -3.0318)
			 (Point3d. 1.7407, -1.3452, -2.0438)],
			"donor" ()}
	feature-ks (keys feature-points)
	feature-points-lists (map feature-points feature-ks)
	alignment (clique-based-point-alignment 0.0 
						feature-points-lists
						feature-points-lists)]
    (is (= [[(Point3d. -0.2793, -2.9642, -0.0718)
	     (Point3d. 1.3747, -3.3622, -3.0318)
	     (Point3d. 1.7407, -1.3452, -2.0438)
	     (Point3d. -2.3723, -2.9332, -0.1578)],
	    []
	    [(Point3d. -0.4481333333333335, 
		       -1.9382000000000006, 
		       -6.334633333333335)],
	    [(Point3d. -0.2793, -2.9642, -0.0718)
	     (Point3d. 1.3747, -3.3622, -3.0318)
	     (Point3d. 1.7407, -1.3452, -2.0438)
	     (Point3d. -2.3723, -2.9332, -0.1578)],
	    []]
	   (:selected-constant alignment)))))
