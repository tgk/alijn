(ns alijn.pairwise-alignment
  (:use [alijn 
	 features 
	 utils 
	 colored-point-alignment 
	 molecule-manipulation
	 math]
	clojure.pprint
	clojure.contrib.generic.functor))

(defn pre      [alignment-result] (neg (:variable-center alignment-result)))
(defn rotation [alignment-result] (:rotation alignment-result))
(defn post     [alignment-result] (:constant-center alignment-result))

(defn extract-features-and-align
  "Extracts features and aligns these. Returns
  alignment results as by colored-point-alignment."
  [threshold constant-molecule variable-molecule charge-limit alignment-score]
  (let [constant-features (find-features constant-molecule charge-limit)
	variable-features (find-features variable-molecule charge-limit)
	constant-features-as-points (fmap (partial map get-point) constant-features)
	variable-features-as-points (fmap (partial map get-point) variable-features)
	feature-ks (keys constant-features-as-points)
	constant-points (map constant-features-as-points feature-ks)
	variable-points (map variable-features-as-points feature-ks)]
    (apply 
     max-key 
     (partial alignment-score constant-features-as-points variable-features-as-points)
     (colored-point-alignment threshold constant-points variable-points))))

; Alignment scores
(defn gaussian-overlap-of-alignment
  [constant-features-as-points variable-features-as-points
   alignment-result]
  (let [moved-variable-features (translate-rotate-translate-feature-points;
				 (pre      alignment-result)
				 (rotation alignment-result)
				 (post     alignment-result)
				 variable-features-as-points)]
    (gaussian-overlap constant-features-as-points moved-variable-features)))

(defn number-of-matched-features-in-alignment
  [constant-features-as-points variable-features-as-points
   alignment-result]
  (count (flatten (:selected-constant alignment-result))))

(defn negative-rmsd-of-alignment
  [constant-features-as-points variable-features-as-points
   alignment-result]
  (- (:rmsd alignment-result)))
  
;;;;  
(defn shift-and-rotate-to-frame 
  "Places a variable molecule in the same reference frame as 
  its features have been in an alignment result."
  [alignment-result variable-molecule]
  (translate-rotate-and-translate-molecule 
   (pre      alignment-result)
   (rotation alignment-result)
   (post     alignment-result)
   variable-molecule))

(comment defn align 
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
  [threshold constant-molecule variable-molecules charge-limit alignment-score]
  (let [alignment-results (map #(extract-features-and-align 
				 threshold constant-molecule % charge-limit alignment-score) 
			       variable-molecules)
	alignment-results (map #(assoc %1 :variable-molecule %2) 
			       alignment-results variable-molecules)
	smallest-rmsd-result (apply min-key :rmsd alignment-results)]
    (shift-and-rotate-to-frame smallest-rmsd-result 
			       (:variable-molecule smallest-rmsd-result))))