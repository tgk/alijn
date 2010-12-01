(ns alijn.multiple-flexible-alignment
  (:use [alijn conformation objective molecule-utils]))

(defn vector-obj-fn 
  [stationary-molecule molecules obj-fn-params]
  (let [{ranges :ranges, 
	 conformations :conformations} (conformation-fn 
					stationary-molecule molecules)
	molecule-obj-fn (objective-fn (cons stationary-molecule molecules) 
				      obj-fn-params)
	obj-fn (fn [v] 
		 (let [confs (conformations v)
		       fitness (molecule-obj-fn confs)]
		   {:conformations confs, :fitness fitness}))]
    {:obj-fn obj-fn
     :ranges ranges}))

(defn multiple-flexible-align
  [stationary-molecule molecules
   obj-fn-params
   optimiser]
  (let [{ranges :ranges, obj-fn :obj-fn} (vector-obj-fn stationary-molecule molecules 
							obj-fn-params)
	best-vector (optimiser (comp :fitness obj-fn) ranges)
	{best-conformations :conformations} (obj-fn best-vector)]
    (map (fn [native conformation] 
	   {:conformation conformation
	    :rmsd-to-native (molecule-rmsd native conformation)})
	 (cons stationary-molecule molecules)
	 best-conformations)))