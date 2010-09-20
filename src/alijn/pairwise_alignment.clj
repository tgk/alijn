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
  [threshold constant-molecule variable-molecule]
  (let [constant-features (find-features constant-molecule)
	variable-features (find-features variable-molecule)
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
    (println "points to be aligned from constant")
    (pprint constant-features-as-points)
    (println "points to be aligned from variable")
    (pprint variable-features-as-points)
    (println "feature rmsd" (:rmsd alignment-result))
    (pprint alignment-result)
    (translate-rotate-and-translate-molecule 
     (neg (:variable-center alignment-result))
     (:rotation alignment-result)
     (:constant-center alignment-result)
     variable-molecule)))