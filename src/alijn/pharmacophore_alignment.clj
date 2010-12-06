(ns alijn.pharmacophore-alignment
  (:use [alijn features conformation objective molecule-utils]))

(defn vector-obj-fn 
  [molecule pharmacophore steric obj-fn-params]
  (let [{ranges :ranges, 
	 conformation :conformation} (single-molecule-conformation-fn 
					molecule)
	 molecule-obj-fn (single-molecule-objective-fn
			  molecule pharmacophore steric obj-fn-params)
	 obj-fn (fn [v] 
		  (let [conf (conformation v)
			fitness (molecule-obj-fn conf)]
		    {:conformation conf, :fitness fitness}))]
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
			 (map #(find-features % (:charge-limit objective-fn-params))
			      pharmacophore-molecules)))
	steric (extract-feature-points 
		(apply merge-with concat
		       (map steric-features pharmacophore-molecules)))
	{obj-fn :obj-fn, ranges :ranges} (vector-obj-fn molecule features steric 
							objective-fn-params)
	alignment-vector (optimiser (comp :fitness obj-fn) ranges)
	{conformation :conformation, fitness :fitness} (obj-fn alignment-vector)]
    {:conformation conformation,
     :fitness fitness,
     :rmsd-to-native (molecule-rmsd molecule conformation)}))
						   