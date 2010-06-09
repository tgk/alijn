(ns alijn.kabsch
  (:import [Jama Matrix]
	   [javax.vecmath Point3d]))

;;; Visualisation for debugging
(defn print-matrix [matrix]
  (let [arr (.getArray matrix)]
    (doseq [i (range (count arr))]
      (do
	(doseq [j (range (count (aget arr i)))]
	  (print (aget arr i j) " "))
	(println)))))

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
; Should be in next clojure.contrib...
(defn indexed [s] (map vector (iterate inc 0) s))

(defn points-to-matrix 
  "Converts N Point3d to an N x 3 matrix."
  [points]
  (let [N (count points)
	matrix (Matrix. N 3)]
    (doseq [[i point] (indexed points)]
      (.set matrix i 0 (.x point))
      (.set matrix i 1 (.y point))
      (.set matrix i 2 (.z point)))
    matrix))

(defn matrix-to-points
  "Converts an N x 3 matrix to N Point3ds."
  [matrix]
  (letfn [(extract-row [i] (Point3d. (.get matrix i 0) (.get matrix i 1) (.get matrix i 2)))]
    (map extract-row (range (.getRowDimension matrix)))))

(def test-point-1 (Point3d. 0 0 0))
(def test-point-2 (Point3d. 1 1 0))
(def test-points [test-point-1 test-point-2])

(comment
(print-matrix (points-to-matrix test-points))
(println (matrix-to-points (points-to-matrix test-points)))
)

;;; Putting it all together
(defn kabsch 
  "Performs Kabschs algorithm for minimising the rmsd between the two point colls.
The result is the new positions for the points in the second point collection.
The points are assumed to be centered around some appropiate center of mass."
  [points-1 points-2]
  (let [P (points-to-matrix points-1)
	Q (points-to-matrix points-2)
	rotation (optimal-rotation P Q)
	Q-rotated (.times Q rotation)
	points-2-transformed (matrix-to-points Q-rotated)]
    points-2-transformed))

;;; Evaluating the overlay
(defn rmsd 
  "Calculates the root mean square deviasion."
  [points-1 points-2]
  (let [distances (map #(.distance %1 %2) points-1 points-2)
	s (reduce + distances)]
    (Math/sqrt s)))

(defn rmsd-of-optimal [points-1 points-2]
  (rmsd points-1 (kabsch points-1 points-2)))

(comment
(println "rmsd of same set:" (rmsd test-points test-points))
(println "rmsd of reversed set:" (rmsd test-points [test-point-2 test-point-1]))
)
