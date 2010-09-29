(ns alijn.numerical-optimiser-pairwise-alignment
  (:import javax.vecmath.Point3d)
  (:use [alijn molecule-manipulation math features]))

; Packing and unpacking for numerical optimisers
(defn pack-rotation-and-translation [rotation translation]
  [(.x rotation)    (.y rotation)    (.z rotation)
   (.x translation) (.y translation) (.z translation)])

(defn unpack-rotation-and-translation [[r-x r-y r-z t-x t-y t-z]]
  [(Point3d. r-x r-y r-z) (Point3d. t-x t-y t-z)])

; Calculation of ranges
(def rotation-ranges (repeat 3 [(- (* 2 Math/PI)) (* 2 Math/PI)]))
(defn translation-ranges [molecule]
  (for [coordinate [#(.x (.getPoint3d %))
		    #(.y (.getPoint3d %))
		    #(.z (.getPoint3d %))]
	:let [vals (map coordinate (.atoms molecule))
	      diff (- (apply max vals) (apply min vals))]]
    [(- diff) diff]))
(defn ranges 
  ([molecule] (concat rotation-ranges (translation-ranges molecule)))
  ([m1 m2] (map (fn [[m1-min m1-max] [m2-min m2-max]] 
		  [(min m1-min m2-min) (max m1-max m2-max)])
		(ranges m1) (ranges m2)))) 

; Objective function
(defn create-objective-fn [charge-limit constant-molecule variable-molecule]
  (let [center (center-of-mass variable-molecule)
	to-origo-matrix (translation-matrix (neg center))
	from-origo-matrix (translation-matrix center)
	constant-features (extract-feature-points (find-features constant-molecule charge-limit))
	variable-features (extract-feature-points (find-features variable-molecule charge-limit))]
    (fn [v]
      (let [[rotation translation] (unpack-rotation-and-translation v)
	    matrix (matrix-product 
		    (translation-matrix translation)
		    from-origo-matrix
		    (rotation-matrix rotation)
		    to-origo-matrix)
	    moved-features (apply-matrix-to-features matrix variable-features)]
	(let [overlap (gaussian-overlap constant-features moved-features)]
	  overlap)))))

(defn align 
  "Aligns two molecules using standard features (no steric).
  The supplied optimiser should expect to be given an objective 
  function and a sequence of ranges to perform optimisation 
  over. 
  Returns moved and rotated copy of variable-molecule."
  [charge-limit optimiser constant-molecule variable-molecule]
  (let [objective-fn (create-objective-fn charge-limit constant-molecule variable-molecule) 
 	optimal-vector (optimiser 
			objective-fn
			(ranges constant-molecule variable-molecule))
	[rotation translation] (unpack-rotation-and-translation optimal-vector)
	center (center-of-mass variable-molecule)
	matrix (matrix-product 
		(translation-matrix translation)
		(translation-matrix center)
		(rotation-matrix rotation)
		(translation-matrix (neg center)))]
    {:value (objective-fn optimal-vector)
     :moved-molecule (apply-matrix-to-molecule matrix variable-molecule)}))

(defn align-with-multiple-variable
  [charge-limit optimiser constant-molecule variable-molecules]
  (:moved-molecule
   (apply max-key :value (map (partial align charge-limit optimiser constant-molecule) variable-molecules))))