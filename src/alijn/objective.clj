(ns alijn.objective
  (:use [alijn features energy]))

(def standard-parameters {:energy-contribution 0.01, :charge-limit 0.5, 
			  :feature-scale 1.0, :steric-scale 0.5})

(defn objective-fn 
  "Creates an (efficient) objective function over the molecules,
  using the given parameters which are a map of 
  :energy-contribution, :charge-limit, :feature-scale, :steric-scale.
  If some values are not supplied, values will default to standard
  parameters."
  [molecules objective-fn-params]
  (let [objective-fn-params (merge standard-parameters objective-fn-params)
	feature-maps (map #(find-features 
			    % (:charge-limit objective-fn-params)) 
			  molecules)
	steric-maps (map steric-features molecules)
	cached-feature-id-maps (map atom-id-from-atom-features 
				    molecules feature-maps)
	cached-steric-id-maps (map atom-id-from-atom-features 
				   molecules steric-maps)]
    (fn [molecules]
      (let [feature-maps (map atom-from-atom-id-features 
			      molecules cached-feature-id-maps)
	    steric-maps (map atom-from-atom-id-features
			     molecules cached-steric-id-maps)
	    feature-points-maps (map extract-feature-points feature-maps)
	    steric-points-maps (map extract-feature-points steric-maps)

	    feature-overlap
	    (combine-gaussian-overlaps
	     (for [feature-points-1 feature-points-maps
		   feature-points-2 feature-points-maps
		   :when (not= feature-points-1 feature-points-2)]
	       (gaussian-overlap 
		feature-points-1 feature-points-2 
		:scale (:feature-scale objective-fn-params))))
	    
	    steric-overlap
	    (combine-gaussian-overlaps
	     (for [steric-points-1 steric-points-maps
		   steric-points-2 steric-points-maps
		   :when (not= steric-points-1 steric-points-2)]
	       (gaussian-overlap 
		steric-points-1 steric-points-2
		:scale (:steric-scale objective-fn-params))))
	    
	    steric-clash
	    (-
	     (*
	      (:energy-contribution objective-fn-params)
	      (reduce + (map steric-clash-energy molecules))))]
	{:feature-overlap feature-overlap
	 :steric-overlap steric-overlap
	 :steric-clash steric-clash
	 :total (+ (:total feature-overlap) 
		   (:total steric-overlap)
		   steric-clash)}))))

(defn single-molecule-objective-fn 
  "Creates an (efficient) objective function over the molecule
  and the pharmacophore point map. Uses same type of parameters as
  objective-fn."
  [molecule pharmacophore-features pharmacophore-steric objective-fn-params]
  (let [objective-fn-params (merge standard-parameters objective-fn-params)
	feature-map (find-features molecule (:charge-limit objective-fn-params))
	steric-map (steric-features molecule)
	cached-feature-ids (atom-id-from-atom-features molecule feature-map)
	cached-steric-ids (atom-id-from-atom-features molecule steric-map)]
    (fn [molecule]
      (let [feature-map (atom-from-atom-id-features molecule cached-feature-ids)
	    steric-map (atom-from-atom-id-features molecule cached-steric-ids)
	    feature-points (extract-feature-points feature-map)
	    steric-points (extract-feature-points steric-map)]
	(+
	 (gaussian-overlap feature-points pharmacophore-features 
			   :scale (:feature-scale objective-fn-params))
	 (gaussian-overlap steric-points pharmacophore-steric
			   :scale (:steric-scale objective-fn-params))
	 (- (* (:energy-contribution objective-fn-params) 
	       (steric-clash-energy molecule))))))))