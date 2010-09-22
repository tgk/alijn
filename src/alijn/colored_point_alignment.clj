(ns alijn.colored-point-alignment
    (:use 
     [alijn kabsch utils combinatorics clique-detection]
     clojure.pprint
     clojure.contrib.profile)
    (:require [clojure.contrib.seq :as seq])
    (:import [javax.vecmath Point3d]))

; Algortihms return type:
(defstruct single-alignment-result 
  :rmsd 
  :constant-center :variable-center
  :rotation
  :selected-constant :selected-variable
  :moved-variable)

; Sceleton alignment method, requires a function for selecting grouped pairs
(defn sceleton-point-alignment
  [select-grouped-pairs constant-points variable-points]
  (apply min-key 
	 :rmsd
	 (for [[selected-constant selected-variable]
	       (select-grouped-pairs constant-points variable-points)]
	   (let [[flat-constant unflat-constant] (flatten-groups selected-constant)
		 [flat-variable unflat-variable] (flatten-groups selected-variable)
		 alignment (prof 
			    :alignment
			    (kabsch-with-translation flat-constant flat-variable))]
	     (struct single-alignment-result
		     (:rmsd alignment)
		     (:constant-center alignment) 
		     (:variable-center alignment) 
		     (:rotation alignment)
		     selected-constant selected-variable
		     (unflat-variable (:moved-variable alignment)))))))

; Exhaustive algorithm:
(defn exhaustive-point-alignment 
  [constant-points variable-points]
  (sceleton-point-alignment 
   all-grouped-pairs
   constant-points variable-points))

; Clique-based algorithm:
(defn wrap-points 
  "Wrap points colored by a nested structure as a seq of points
  with a :color and a :elm entry, where :color is their group number."
  [nested-points]
  (apply 
   concat
   (for [[color points] (seq/indexed nested-points)]
     (for [p points] {:elm p :color color}))))

(defn unwrap-points 
  "Unwrap a seq of points and transform them into a nested vector based on
  their :color."
  [colors colored-points]
  (if (empty? colored-points)
    []
    (let [grouped (group-by :color colored-points)]
      (for [color colors] 
	(map :elm (get grouped color []))))))
  
(defn clique-grouped-pairs [threshold constant-points variable-points]
  (let [cons-wrapped (wrap-points constant-points)
	vari-wrapped (wrap-points variable-points)
	max-color (count constant-points)
	colors (range max-color)
	corr-graph (prof :corr-graph (correspondance-graph-from-colored-points 
				      threshold cons-wrapped vari-wrapped))
	biggest-pairing (prof :biggest-pairing (apply max-key (comp count flatten) 
						      (possible-pairings corr-graph)))
	pairings [biggest-pairing]]
    (for [[cons-lineup vari-lineup] pairings]
      [(unwrap-points colors cons-lineup)
       (unwrap-points colors vari-lineup)])))

(defn clique-based-point-alignment
  [threshold constant-points variable-points]
  (sceleton-point-alignment
   (partial clique-grouped-pairs threshold)
   constant-points variable-points))

; Main function
(defn colored-point-alignment
  "Aligns colored points under a common translation and rotation.
  If threhold is false, all constant points of each color are paired
  with all variable points of the same color and the pairing resulting 
  in the lowest rmsd is returned. If one color has more points in 
  constant-points than variable-points (or vice versa) as many points
  as possible are matched and the remaning points are ignored.
  If threshold is a numerical value a clique algorithm is used to
  descriminate which points can be matched to which points according
  to the methods described by Willett et. al.
  constant-points and variable-points contain the points grouped 
  by their color. For example, if we have the colors red, green 
  and blue, the vector [[r1, r2] [g], [b1, b2, b3]] corresponds to a 
  set of two blue, one green and three blue points.
  A point is a Point3d object."
  [threshold constant-points variable-points]
  (if (number? threshold)
    (clique-based-point-alignment threshold constant-points variable-points)
    (exhaustive-point-alignment constant-points variable-points)))