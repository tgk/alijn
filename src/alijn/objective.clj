(ns alijn.objective
  (:use [alijn features energy]))

(defn objective-fn 
  "Creates an (efficient) objective function over the molecules,
  using the given parameters which are a map of 
  :energy-contribution, :charge-limit, :feature-scale, :steric-scale.
  If some values are not supplied, values will default to standard
  parameters."
  [molecules objective-fn-params]
  (let [objective-fn-params (merge 
			     {:energy-contribution 0.01
			      :charge-limit 0.5
			      :feature-scale 1.0
			      :steric-scale 0.5}
			     objective-fn-params)
	feature-maps (map #(find-features % (:charge-limit objective-fn-params)) 
			  molecules)
	steric-maps (map steric-features molecules)
	cached-feature-id-maps (map atom-id-from-atom-features molecules feature-maps)
	cached-steric-id-maps (map atom-id-from-atom-features molecules steric-maps)]
    (fn [molecules]
      (let [feature-maps (map atom-from-atom-id-features 
			      molecules cached-feature-id-maps)
	    steric-maps (map atom-from-atom-id-features
			     molecules cached-steric-id-maps)
	    feature-points-maps (map extract-feature-points feature-maps)
	    steric-points-maps (map extract-feature-points steric-maps)]
	(+
	 (reduce 
	  +
	  (for [feature-points-1 feature-points-maps
		feature-points-2 feature-points-maps
		:when (not= feature-points-1 feature-points-2)]
	    (gaussian-overlap feature-points-1 feature-points-2 
			      :scale (:feature-scale objective-fn-params))))
	 (reduce 
	  +
	  (for [steric-points-1 steric-points-maps
		steric-points-2 steric-points-maps
		:when (not= steric-points-1 steric-points-2)]
	    (gaussian-overlap steric-points-1 steric-points-2
			      :scale (:steric-scale objective-fn-params))))
	 (-
	  (*
	   (:energy-contribution objective-fn-params)
	   (reduce + (map steric-overlap molecules)))))))))