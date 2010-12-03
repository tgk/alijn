(ns alijn.pharmacophore-alignment
  (:use [alijn conformation objective molecule-utils]))

(defn vector-obj-fn 
  [molecule pharmacophore steric obj-fn-params]
  (let [{ranges :ranges, 
	 conformation :conformation} (single-molecule-conformation-fn 
					molecule)
	 molecule-obj-fn (single-molecule-objective-fn
			  pharmacophore steric obj-fn-params)
	 obj-fn (fn [v] 
		  (let [conf (conformation v)
			fitness (molecule-obj-fn conf)]
		    {:conformation conf, :fitness fitness}))]
    {:obj-fn obj-fn
     :ranges ranges}))

(defn align-molecule-to-pharmacophore
  "Assumes that both pharmacophore and steric are maps to Point3d objects."
  [pharmacophore
   steric
   molecule
   optimiser
   objective-fn-params]
  (let [{obj-fn :obj-fn, ranges :ranges} (vector-obj-fn molecule pharmacophore steric 
							objective-fn-params)
	alignment-vector (optimiser (comp :fitness obj-fn) ranges)
	{conformation :conformation, fitness :fitness} (obj-fn alignment-vector)]
    {:conformation conformation,
     :fitness fitness,
     :rmsd-to-native (molecule-rmsd molecule conformation)}))
						   