(ns alijn.colored-point-alignment
    (:use [alijn kabsch utils]))

; Algortihms return type:
(defstruct single-alignment-result 
  :rmsd 
  :translation :rotation
  :selected-constant :selected-variable
  :moved-variable)

; Exhaustive algorithm:
(defn exhaustive-point-alignment
  [constant-points variable-points]
  (apply min-key 
	 :rmsd
	 (for [[selected-constant selected-variable]
	       (all-grouped-pairs constant-points variable-points)]
	   (let [[flat-constant unflat-constant] (flatten-group constant-points)
		 [flat-variable unflat-variable] (flatten-group variable-points)
		 alignment (kabsch-with-translation flat-constant flat-variable)]
	     (struct (:rmsd alignment)
		     (:translation alignment) (:rotation alignment)
		     selected-constant selected-variable
		     (unflat-variable (:rotated-points alignment)))))))

; Clique-based algorithm:
(defn- clique-matches 
  [threshold colored-points-1 colored-points-2])

(defn clique-based-point-alignment
  [threshold contant-points variable-points])

; Wrapper for both methods
; Multimedthods for threshold vs. no threshold?
; Can always refactor for that, forget for now
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
set of two blue, one green and three green points.
A point is a Point3d object."
  [threshold constant-points variable-points]
  (if threshold
    (clique-based-point-alignment threshold constant-points variable-points)
    (exhaustive-point-alignment constant-points variable-points)))