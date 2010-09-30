(ns alijn.numerical-optimiser-pairwise-alignment
  (:import javax.vecmath.Point3d)
  (:use [alijn molecule-manipulation math features rotation-tree]))

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
(def dihedral-angle-range [0 (* 2 Math/PI)])

; Objective function
(defn create-objective-fn 
  [flexible-dihedral?
   charge-limit 
   feature-scale steric-scale
   constant-molecule variable-molecule]
  (let [rotation-tree (when flexible-dihedral? (calculate-rotation-tree variable-molecule))
	d-o-f (if flexible-dihedral? (:degrees-of-freedom rotation-tree) 0)
	center (center-of-mass variable-molecule)
	to-origo-matrix (translation-matrix (neg center))
	from-origo-matrix (translation-matrix center)
	constant-features (extract-feature-points (find-features constant-molecule charge-limit))
	constant-steric   (extract-feature-points (steric-features constant-molecule))
	variable-features-ids (atom-id-from-atom-features 
			       variable-molecule (find-features variable-molecule charge-limit))
	variable-steric-ids   (atom-id-from-atom-features 
			       variable-molecule (steric-features variable-molecule))]
    {:ranges (concat (ranges constant-molecule variable-molecule) (repeat d-o-f dihedral-angle-range))
     :objective-fn
     (fn [v]
       (let [variable-molecule (if flexible-dihedral?
				 (molecule-configuration rotation-tree (drop 6 v))
				 variable-molecule)
	     [rotation translation] (unpack-rotation-and-translation (take 6 v))
	     matrix (matrix-product 
		     (translation-matrix translation)
		     from-origo-matrix
		     (rotation-matrix rotation)
		     to-origo-matrix)
	     moved-molecule (apply-matrix-to-molecule matrix variable-molecule)
	     moved-features (extract-feature-points 
			     (atom-from-atom-id-features moved-molecule variable-features-ids))
	     moved-steric   (extract-feature-points
			     (atom-from-atom-id-features moved-molecule variable-steric-ids))]
	 (let [overlap (+ (gaussian-overlap constant-features moved-features :scale feature-scale)
			  (gaussian-overlap constant-steric   moved-steric   :scale steric-scale))]
	   {:overlap overlap
	    :moved-molecule moved-molecule})))}))

(defn align 
  "Aligns two molecules using standard features (no steric).
  The supplied optimiser should expect to be given an objective 
  function and a sequence of ranges to perform optimisation 
  over. 
  Returns moved and rotated copy of variable-molecule."
  [flexible-dihedral?
   charge-limit 
   feature-scale steric-scale
   optimiser 
   constant-molecule variable-molecule]
  (let [variable-molecule (randomise-molecule-orientation (randomise-molecule-orientation variable-molecule))
	variable-molecule (move-molecule-center variable-molecule (center-of-mass constant-molecule))
	{objective-fn :objective-fn, ranges :ranges} 
	(create-objective-fn flexible-dihedral? 
			     charge-limit 
			     feature-scale steric-scale
			     constant-molecule variable-molecule) 
 	optimal-vector (optimiser (comp :overlap objective-fn) ranges)
	{value :overlap, moved-molecule :moved-molecule} (objective-fn optimal-vector)]
    {:value value, :moved-molecule moved-molecule, :degrees-of-freedom (count ranges)}))

(defn align-with-multiple-variable
  [flexible-dihedral?
   charge-limit 
   feature-scale steric-scale
   optimiser 
   constant-molecule variable-molecules]
  (apply max-key :value (map (partial 
			       align
			       flexible-dihedral?
			       charge-limit 
			       feature-scale steric-scale
			       optimiser constant-molecule) variable-molecules)))