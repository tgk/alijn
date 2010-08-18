(ns alijn.math
  (:use [clj-todo])
  (:import [javax.vecmath Point3d]))

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