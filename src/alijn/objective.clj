(ns alijn.objective
  (:use [alijn features energy]))

(defn objective-fn 
  "Creates an (efficient) objective function over the molecules,
  using the given parameters which are a map of 
  :energy-contribution, :charge-limit, :feature-scale, :steric-scale.
  If some values are not supplied, values will default to standard
  parameters."
  [molecules objective-fn-params]
  (let [feature-maps (map #(find-features 
			    % (:charge-limit objective-fn-params)) 
			  molecules)
	cached-feature-id-maps (map atom-id-from-atom-features 
				    molecules feature-maps)]
    (fn [molecules]
      (let [feature-maps (map atom-from-atom-id-features 
			      molecules cached-feature-id-maps)
	    feature-points-maps (map extract-feature-points feature-maps)
	    feature-overlap
	    (combine-gaussian-overlaps
	     (for [feature-points-1 feature-points-maps
		   feature-points-2 feature-points-maps
		   :when (not= feature-points-1 feature-points-2)]
	       (gaussian-overlap 
		feature-points-1 feature-points-2 
		(:feature-parameters objective-fn-params))))
	    steric-clash
	    (- (* (:energy-contribution objective-fn-params)
		  (reduce + (map steric-clash-energy molecules))))]
	{:feature-overlap feature-overlap
	 :steric-clash steric-clash
	 :total (+ (:total feature-overlap) steric-clash)}))))

(defn single-molecule-objective-fn 
  "Creates an (efficient) objective function over the molecule
  and the pharmacophore point map. Uses same type of parameters as
  objective-fn."
  [molecule pharmacophore-features objective-fn-params]
  (let [feature-map (find-features 
		     molecule (:charge-limit objective-fn-params))
	cached-feature-ids (atom-id-from-atom-features 
			    molecule feature-map)]
    (fn [molecule]
      (let [feature-map (atom-from-atom-id-features 
			 molecule cached-feature-ids)
	    feature-points (extract-feature-points feature-map)]
	{:feature-overlap (gaussian-overlap 
			   feature-points pharmacophore-features 
			   (:feature-parameters objective-fn-params))
	 :steric-clash (- (* (:energy-contribution objective-fn-params) 
			     (steric-clash-energy molecule)))}))))