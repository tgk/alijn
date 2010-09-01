(ns alijn.kabsch
  (:use clj-todo
	alijn.math)
  (:require [clojure.contrib.seq :as seq])
  (:import [Jama Matrix]
	   [javax.vecmath Point3d]))

;;; Calculations
(defn calculate-A 
  "Calculates A = P transposed X Q"
  [P Q]
  (.times (.transpose P) Q))

(defn calculate-SVD
  "Calculates the singular value decomposition, returns left and right matrix"
  [A]
  (let [svd (.svd A)
	V (.getU svd)
	W (.getV svd)]
    [V W]))

(defn sign 
  "Calculates the sign of the determinant of the matrix."
  [matrix]
  (if (> (.det matrix) 0) 1 -1))

(defn sign-matrix [matrix]
  (let [d (sign matrix)
	result (Matrix/identity 3 3)]
    (.set result 2 2 d)
    result))

(defn optimal-rotation
  "Calculates the optimal rotation using singular value decomposition."
  [P Q]
  (let [A (calculate-A P Q)
	[V W] (calculate-SVD A)]
    (reduce 
     #(.times %1 %2)
     [W (sign-matrix A) (.transpose V)])))

;;; Conversion functions
(defn points-to-matrix 
  "Converts N Point3d to an N x 3 matrix."
  [points]
  (let [N (count points)
	matrix (Matrix. N 3)]
    (doseq [[i point] (seq/indexed points)]
      (.set matrix i 0 (.x point))
      (.set matrix i 1 (.y point))
      (.set matrix i 2 (.z point)))
    matrix))

(defn matrix-to-points
  "Converts an N x 3 matrix to N Point3ds."
  [matrix]
  (letfn [(extract-row [i] (Point3d. (.get matrix i 0) (.get matrix i 1) (.get matrix i 2)))]
    (map extract-row (range (.getRowDimension matrix)))))

;;; Kabsch
(defn kabsch 
  "Performs Kabschs algorithm for minimising the rmsd between the two point colls.
The result is the new positions for the points in the second point collection.
The points are assumed to be centered around some appropiate center of mass."
  [constant-points variable-points]
  (let [P (points-to-matrix constant-points)
	Q (points-to-matrix variable-points)
	rotation (optimal-rotation P Q)
	Q-rotated (.times Q rotation)
	variable-points-transformed (matrix-to-points Q-rotated)
	rotation-rmsd (rmsd constant-points variable-points-transformed)]
    {:rotation rotation,
     :rotated-points variable-points-transformed,
     :rmsd rotation-rmsd}))

;;; Translated Kabsch
(defn kabsch-with-translation
  "Performs Kabsch algorithm, but first centers the two set of points
around zero according to their center of mass.
The moved and rotated set of variable points is then moved back into 
the constant set of points center of mass afterwards."
  [constant-points variable-points]
  (let [constant-center (vec-center constant-points)
	variable-center (vec-center variable-points)
	constant-translated (move-points constant-points (neg constant-center))
	variable-translated (move-points variable-points (neg variable-center))
	rotated (kabsch constant-translated variable-translated)
	variable-in-same-frame-as-constant (move-points
					    (:rotated-points rotated)
					    constant-center)]
    {:rotation (:rotation rotated)
     :rmsd (:rmsd rotated)
     :constant-center constant-center
     :variable-center variable-center
     :moved-variable variable-in-same-frame-as-constant}))