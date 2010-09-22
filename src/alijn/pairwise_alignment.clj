(ns alijn.pairwise-alignment
  (:use [alijn 
	 features 
	 utils 
	 colored-point-alignment 
	 molecule-manipulation
	 math]
	clojure.pprint
	clojure.contrib.generic.functor))

(defn extract-features-and-align
  "Extracts features and aligns these. Returns
  alignment result as by colored-point-alignment."
  [threshold constant-molecule variable-molecule charge-limit]
  (let [constant-features (find-features constant-molecule charge-limit)
	variable-features (find-features variable-molecule charge-limit)
	constant-features-as-points (fmap (partial map get-point) constant-features)
	variable-features-as-points (fmap (partial map get-point) variable-features)
	feature-ks (keys constant-features-as-points)
	constant-points (map constant-features-as-points feature-ks)
	variable-points (map variable-features-as-points feature-ks)]
    (colored-point-alignment threshold constant-points variable-points)))
  
(defn shift-and-rotate-to-frame 
  "Places a variable molecule in the same reference frame as 
  its features have been in an alignment result."
  [alignment-result variable-molecule]
  (translate-rotate-and-translate-molecule 
   (neg (:variable-center alignment-result))
   (:rotation alignment-result)
   (:constant-center alignment-result)
   variable-molecule))

(defn align 
  "Aligns a pair of molecules using their features.
  Returns variable-molecule shifted and rotated to
  match features of constant-molecule."
  [threshold constant-molecule variable-molecule charge-limit]
  (let [alignment-result (extract-features-and-align 
			  threshold 
			  constant-molecule variable-molecule 
			  charge-limit)]
    (shift-and-rotate-to-frame alignment-result variable-molecule)))

(defn align-with-multiple-variable
  "Aligns a single molecule with all the conformations
  of the variable molecule using features. Returns the
  shifted and rotated smallest molecule that gave rise
  to the lowest rmsd."
  [threshold constant-molecule variable-molecules charge-limit]
  (let [alignment-results (map #(extract-features-and-align 
				 threshold constant-molecule % charge-limit) 
			       variable-molecules)
	alignment-results (map #(assoc %1 :variable-molecule %2) 
			       alignment-results variable-molecules)
	smallest-rmsd-result (apply min-key :rmsd alignment-results)]
    (shift-and-rotate-to-frame smallest-rmsd-result 
			       (:variable-molecule smallest-rmsd-result))))