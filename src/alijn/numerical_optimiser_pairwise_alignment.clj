(ns alijn.numerical-optimiser-pairwise-alignment
  (:import javax.vecmath.Point3d)
  (:use [alijn molecule-manipulation math features rotation-tree energy conformation]
	clojure.contrib.profile))

(defn ranges 
  ([molecule] (concat rotation-ranges (translation-ranges molecule)))
  ([m1 m2] (map (fn [[m1-min m1-max] [m2-min m2-max]] 
		  [(min m1-min m2-min) (max m1-max m2-max)])
		(ranges m1) (ranges m2)))) 

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
       (prof :objective-fn
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
	    :moved-molecule moved-molecule}))))}))

(defn align 
  "Aligns two molecules using standard features (no steric).
  The supplied optimiser should expect to be given an objective 
  function and a sequence of ranges to perform optimisation 
  over. 
  Returns moved and rotated copy of variable-molecule."
  [objective-fn-params
   optimiser 
   constant-molecule variable-molecule]
  (prof 
   :align
  (let [variable-molecule (randomise-molecule-orientation 
			   (randomise-molecule-orientation 
			    variable-molecule))
	variable-molecule (move-molecule-center 
			   variable-molecule 
			   (center-of-mass constant-molecule))
	{objective-fn :objective-fn, ranges :ranges} 
	(create-objective-fn objective-fn-params constant-molecule variable-molecule) 
;	objective-fn (memoize-visible-atom objective-fn)
 	optimal-vector (optimiser (comp :fitness objective-fn) ranges)
	{value :fitness, moved-molecule :moved-molecule} (objective-fn optimal-vector)]
    {:value value
     :moved-molecule moved-molecule
     :degrees-of-freedom (count ranges)})))

(defn align-with-multiple-variable
  [objective-fn-params
   optimiser 
   constant-molecule variable-molecules]
  (apply 
   max-key :value 
   (map (partial align objective-fn-params optimiser constant-molecule) 
	variable-molecules)))