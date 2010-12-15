(ns alijn.pharmacophore-alignment
  (:use [alijn 
	 features conformation objective 
	 molecule-utils fitness features]))

(defrecord
  PharmacophoreAlignmentFitness
  [feature-overlap steric-clash conformation]
  Fitness
  (value [this] (+ (:total feature-overlap)
		   steric-clash))
  (string-rep [this] (str (value this)
			  " "
			  (gaussian-overlap-string-rep feature-overlap)
			  "ster-clash " steric-clash)))

(defn vector-obj-fn 
  [molecule pharmacophore obj-fn-params]
  (let [{ranges :ranges, 
	 conformation :conformation} (single-molecule-conformation-fn 
					molecule)
	 molecule-obj-fn (single-molecule-objective-fn
			  molecule pharmacophore obj-fn-params)
	 obj-fn (fn [v] 
		  (let [conf (conformation v)
			fitness (molecule-obj-fn conf)]
		    (PharmacophoreAlignmentFitness.
		     (:feature-overlap fitness)
		     (:steric-clash fitness)
		     conf)))]
    {:obj-fn obj-fn
     :ranges ranges}))

(defn align-molecule-to-pharmacophore
  "Assumes that both pharmacophore and steric are maps to Point3d objects."
  [pharmacophore-molecules
   molecule
   optimiser
   objective-fn-params]
  (let [features (extract-feature-points
		  (apply merge-with concat 
			 (map #(find-features 
				% (:charge-limit objective-fn-params))
			      pharmacophore-molecules)))
	{obj-fn :obj-fn, ranges :ranges} (vector-obj-fn 
					  molecule features 
					  objective-fn-params)
	alignment-vector (optimiser obj-fn ranges)
	fitness (obj-fn alignment-vector)]
    {:conformation (:conformation fitness),
     :fitness (value fitness),
     :rmsd-to-native (molecule-rmsd molecule (:conformation fitness))}))
						   