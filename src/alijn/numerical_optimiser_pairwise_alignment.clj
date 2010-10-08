(ns alijn.numerical-optimiser-pairwise-alignment
  (:import javax.vecmath.Point3d)
  (:use [alijn molecule-manipulation math features rotation-tree energy]))

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
(defrecord Objective-fn-params
  [flexible-dihedral?
   energy-contribution
   charge-limit
   feature-scale
   steric-scale])

(defn create-objective-fn 
  [objective-fn-params
   constant-molecule variable-molecule]
  (let [rotation-tree-constant (when (:flexible-dihedral? objective-fn-params) 
				 (calculate-rotation-tree constant-molecule))
	rotation-tree-variable (when (:flexible-dihedral? objective-fn-params) 
				 (calculate-rotation-tree variable-molecule))
	d-o-f (if (:flexible-dihedral? objective-fn-params)
		(+ (:degrees-of-freedom rotation-tree-constant) 
		   (:degrees-of-freedom rotation-tree-variable))
		0)
	center (center-of-mass variable-molecule)
	to-origo-matrix (translation-matrix (neg center))
	from-origo-matrix (translation-matrix center)
	constant-features-ids (atom-id-from-atom-features 
			       constant-molecule 
			       (find-features constant-molecule 
					      (:charge-limit objective-fn-params)))
	constant-steric-ids   (atom-id-from-atom-features 
			       constant-molecule 
			       (steric-features constant-molecule))
	variable-features-ids (atom-id-from-atom-features 
			       variable-molecule 
			       (find-features variable-molecule 
					      (:charge-limit objective-fn-params)))
	variable-steric-ids   (atom-id-from-atom-features 
			       variable-molecule 
			       (steric-features variable-molecule))]
    {:ranges (concat (ranges constant-molecule variable-molecule) 
		     (repeat d-o-f dihedral-angle-range))
     :objective-fn
     (fn [v]
       (let [constant-molecule (if (:flexbile-dihedral? objective-fn-params)
				 (molecule-configuration 
				  rotation-tree-constant 
				  (take (:degrees-of-freedom rotation-tree-constant) 
					(drop 6 v)))
				 constant-molecule)
	     variable-molecule (if (:flexible-dihedral? objective-fn-params)
				 (molecule-configuration 
				  rotation-tree-variable 
				  (drop 
				   (+ 6 (:degrees-of-freedom rotation-tree-constant)) 
				   v))
				 variable-molecule)
	     constant-features (extract-feature-points
				(atom-from-atom-id-features
				 constant-molecule constant-features-ids))
	     constant-steric (extract-feature-points
			      (atom-from-atom-id-features
			       constant-molecule constant-steric-ids))
	     [rotation translation] (unpack-rotation-and-translation (take 6 v))
	     matrix (matrix-product 
		     (translation-matrix translation)
		     from-origo-matrix
		     (rotation-matrix rotation)
		     to-origo-matrix)
	     moved-molecule (apply-matrix-to-molecule matrix variable-molecule)
	     moved-features (extract-feature-points 
			     (atom-from-atom-id-features 
			      moved-molecule variable-features-ids))
	     moved-steric   (extract-feature-points
			     (atom-from-atom-id-features 
			      moved-molecule variable-steric-ids))]
	 (let [overlap (+ (gaussian-overlap 
			   constant-features moved-features 
			   :scale (:feature-scale objective-fn-params))
			  (gaussian-overlap 
			   constant-steric   moved-steric   
			   :scale (:steric-scale objective-fn-params)))
	       energy (if (= 0 (:energy-contribution objective-fn-params)) 
			0 
			(+ (steric-overlap variable-molecule)
			   (steric-overlap constant-molecule)))
	       fitness (- overlap 
			  (* (:energy-contribution objective-fn-params) energy))]
	   {:overlap overlap
	    :energy energy
	    :fitness fitness
	    :moved-molecule moved-molecule})))}))

(defn align 
  "Aligns two molecules using standard features (no steric).
  The supplied optimiser should expect to be given an objective 
  function and a sequence of ranges to perform optimisation 
  over. 
  Returns moved and rotated copy of variable-molecule."
  [objective-fn-params
   optimiser 
   constant-molecule variable-molecule]
  (let [variable-molecule (randomise-molecule-orientation 
			   (randomise-molecule-orientation 
			    variable-molecule))
	variable-molecule (move-molecule-center 
			   variable-molecule 
			   (center-of-mass constant-molecule))
	{objective-fn :objective-fn, ranges :ranges} 
	(create-objective-fn objective-fn-params constant-molecule variable-molecule) 
 	optimal-vector (optimiser (comp :overlap objective-fn) ranges)
	{value :fitness, moved-molecule :moved-molecule} (objective-fn optimal-vector)]
    {:value value
     :moved-molecule moved-molecule
     :degrees-of-freedom (count ranges)}))

(defn align-with-multiple-variable
  [objective-fn-params
   optimiser 
   constant-molecule variable-molecules]
  (apply 
   max-key :value 
   (map (partial align objective-fn-params optimiser constant-molecule) 
	variable-molecules)))