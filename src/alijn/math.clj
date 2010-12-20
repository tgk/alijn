(ns alijn.math
  (:import [javax.vecmath Point3d]
	   [Jama Matrix]))

(defn vec-length [u]
  (Math/sqrt 
   (+
    (Math/pow (.x u) 2)
    (Math/pow (.y u) 2)
    (Math/pow (.z u) 2))))

(defn normalised [u]
  (let [l (vec-length u)
	clone (Point3d. u)]
    (do
      (set! (.x clone) (/ (.x clone) l))
      (set! (.y clone) (/ (.y clone) l))
      (set! (.z clone) (/ (.z clone) l))
      clone)))
    
(defn vec-add 
  "u + v, where u and v are Point3d vectors."
  [u v] (doto (Point3d.) (.add u v)))

(defn vec-sub 
  "u - v, where u and v are Point3d vectors."
  [u v] (doto (Point3d.) (.sub u v)))

(defn neg [u] (vec-sub (Point3d. 0 0 0) u))

(defn vec-center 
  "Finds the vector center for a seq of Point3d."
  [points]
  (let [result (Point3d. 0 0 0)]
    (assert (> (count points) 0))
    (doseq [point points] (.add result point))
    (.scale result (/ 1 (count points)))
    result))
  
(defn move-points
  [points translation]
  (map (partial vec-add translation) points))

(defn matrix-vector-product
  [matrix vector]
  (let [x (.x vector), y (.y vector), z (.z vector)
	get-row (fn [i] (map #(.get matrix i %) (range 3)))
	get-entry (fn [matrix-row] (reduce + (map * [x y z] (get-row matrix-row))))]
    (Point3d. (get-entry 0)
	      (get-entry 1)
	      (get-entry 2))))

(defn rotate-point
  [rotation point]
  (matrix-vector-product rotation point))

(defn distance [u v] (.distance u v))
(defn distance-squared [u v] (.distanceSquared u v))

(defn average [coll]
  (/ (reduce + coll) (count coll)))

(defn rmsd 
  "Calculates the root mean square deviation."
  [points-1 points-2]
  (assert (= (count points-1) (count points-2)))
  (-> (map distance-squared points-1 points-2) 
      average
      Math/sqrt))

;;; Matrix operations
(defn matrix-product 
  ([m] m)
  ([m & ms] (.times m (apply matrix-product ms))))

;;; 4D matrix
(defn rotation-matrix 
  "axis is assumed to be normalised.
  If only axis is given it will be normalised and its magnitude will be
  used as angle."
  ([axis] (rotation-matrix (vec-length axis) (normalised axis)))
  ([angle axis]
     (let [c (Math/cos angle), s (Math/sin angle), omc (- 1 c) ;one minus c
	   x (.x axis), y (.y axis), z (.z axis)
	   xs (* x s), ys (* y s), zs (* z s)
	   xyomc (* x y omc), xzomc (* x z omc), yzomc (* y z omc)]
       (Matrix. 
	(double-array
	 [(+ (* x x omc) c)  (+ xyomc zs)       (- xzomc ys)       0
	  (- xyomc zs)       (+ (* y y omc) c)  (+ yzomc xs)       0
	  (+ xzomc ys)       (- yzomc xs)       (+ (* z z omc) c)  0
	  0                  0                  0                  1])
	4))))

(defn translation-matrix [translation]
  (doto (Matrix/identity 4 4)
    (.set 0 3 (.x translation))
    (.set 1 3 (.y translation))
    (.set 2 3 (.z translation))))

(defn move-and-translate-point [matrix point]
  (let [point-matrix (doto (Matrix. 4 1)
		       (.set 0 0 (.x point))
		       (.set 1 0 (.y point))
		       (.set 2 0 (.z point))
		       (.set 3 0 1.0))
	moved-matrix (.times matrix point-matrix)]
    (Point3d. (.get moved-matrix 0 0)
	      (.get moved-matrix 1 0)
	      (.get moved-matrix 2 0))))

