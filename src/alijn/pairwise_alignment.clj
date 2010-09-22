(ns alijn.pairwise-alignment
  (:use [alijn 
	 custom-features 
	 utils 
	 colored-point-alignment 
	 molecule-manipulation
	 math]
	clojure.pprint))

(defn align 
  "Aligns a pair of molecules using their features.
  Returns variable-molecule shifted and rotated to
  match features of constant-molecule."
  [threshold constant-molecule variable-molecule charge-limit]
  (let [constant-features (find-features constant-molecule charge-limit)
	variable-features (find-features variable-molecule charge-limit)
	constant-features-as-points (map-on-values (partial map get-point) 
						   constant-features)
	variable-features-as-points (map-on-values (partial map get-point)
						   variable-features)
	feature-ks (keys constant-features-as-points)
	constant-points (map constant-features-as-points feature-ks)
	variable-points (map variable-features-as-points feature-ks)
	alignment-result (colored-point-alignment 
			  threshold 
			  constant-points variable-points)]
    (translate-rotate-and-translate-molecule 
     (neg (:variable-center alignment-result))
     (:rotation alignment-result)
     (:constant-center alignment-result)
     variable-molecule)))