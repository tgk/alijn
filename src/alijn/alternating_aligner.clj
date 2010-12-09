(ns alijn.alternating-aligner
  (:use [alijn conformation objective molecule-utils]))

(defn vector-obj-fns
  [stationary-molecule molecules obj-fn-params]
  (let [{ranges :ranges 
	 conformations :conformations 
	 sub-conformations :sub-conformations-fn} (alternating-aligner-conformation-fn 
						   stationary-molecule molecules)
	 molecules-obj-fn (objective-fn 
			   (cons stationary-molecule molecules) obj-fn-params)
	 obj-fn (fn [v] 
		  (let [confs (conformations v)
			fitness (molecules-obj-fn confs)]
		    {:conformations confs, :fitness fitness}))
	 sub-obj-fn (fn [i v]
		      (let [{sub-ranges :sub-ranges
			     full-vector :full-vector
			     sub-conf :sub-conformations} (sub-conformations i v)]
			{:obj-fn (comp molecules-obj-fn sub-conf)
			 :ranges sub-ranges
			 :full-vector full-vector}))]
    {:obj-fn obj-fn
     :ranges ranges
     :sub-obj-fn sub-obj-fn}))

(defn local-align 
  [optimiser sub-obj-fn initial-vector molecule-count]
  (loop [i 1
	 v initial-vector]
    (if (> i molecule-count)
      v
      (let [{obj-fn :obj-fn, ranges :ranges, full-vector :full-vector} 
	    (sub-obj-fn i v)]
	(recur (inc i) (full-vector (optimiser obj-fn ranges)))))))
	 
(defn alternating-align
  [stationary-molecule molecules
   obj-fn-params
   pre-optimiser middle-optimiser post-optimiser]
  (let [{obj-fn :obj-fn
	 ranges :ranges
	 sub-obj-fn :sub-obj-fn} (vector-obj-fns 
				  stationary-molecule molecules obj-fn-params)
	 best-after-first (pre-optimiser (comp :fitness obj-fn) ranges)
	 best-after-middle (local-align 
			    middle-optimiser sub-obj-fn 
			    best-after-first (count molecules))
	 best-after-last (post-optimiser 
			  (comp :fitness obj-fn) 
			  (map (fn [v] [(- v 1) (+ v 1)]) best-after-middle))
	 {best-conformations :conformations} (obj-fn best-after-last)]
    (map (fn [native conformation] 
	   {:conformation conformation
	    :rmsd-to-native (molecule-rmsd native conformation)})
	 (cons stationary-molecule molecules)
	 best-conformations)))