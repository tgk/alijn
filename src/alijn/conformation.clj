(ns alijn.conformation
  (:import javax.vecmath.Point3d)  
  (:use [alijn rotation-tree molecule-manipulation math]))

; Packing and unpacking for numerical optimisers
(defn pack-rotation-and-translation [rotation translation]
  [(.x rotation)    (.y rotation)    (.z rotation)
   (.x translation) (.y translation) (.z translation)])

(defn unpack-rotation-and-translation [[r-x r-y r-z t-x t-y t-z]]
  [(Point3d. r-x r-y r-z) (Point3d. t-x t-y t-z)])


; Calculation of ranges
(def rotation-ranges (repeat 3 [(- (* 2 Math/PI)) (* 2 Math/PI)]))

(defn translation-ranges [molecule]
  (for [coordinate [#(.x (.getPoint3d %))
		    #(.y (.getPoint3d %))
		    #(.z (.getPoint3d %))]
	:let [vals (map coordinate (.atoms molecule))
	      diff (- (apply max vals) (apply min vals))]]
    [(- diff) diff]))

(def dihedral-angle-range [0 (* 2 Math/PI)])


; Another packing scheme
(defn partition-using 
  "Partitions coll into chunks of size n1, n2, n3, ... ."
  [ns coll]
  (assert (= (reduce + ns) (count coll)))
  (loop [ns ns, coll coll, result []]
    (if (empty? coll)
      (reverse result)
      (recur (rest ns) 
	     (drop (first ns) coll)
	     (cons (take (first ns) coll) result)))))

(defn unpack-point [[x y z]] (Point3d. x y z))

(defn unpack-n-points [n v]
  (map unpack-point (partition-using (repeat n 3) v)))

(defn unpack-molecules 
  "Unpacks points for n molecules and one stationary molecule.
  The returned data is on the form [dihedral-angles translations rotations].
  There are n+1 lists in dihedral-angles and n Point3ds in translations and 
  rotations."
  [dihedral-angles-counts v]
  (let [n (dec (count dihedral-angles-counts))
	total-dihedral-angles (reduce + dihedral-angles-counts)
	[dihedral-angles-part 
	 translations-part 
	 rotations-part] (partition-using 
			  [total-dihedral-angles (* n 3) (* n 3)] v)]
    [(partition-using dihedral-angles-counts dihedral-angles-part)
     (unpack-n-points n translations-part)
     (unpack-n-points n rotations-part)]))

(defn unpack-molecule
  "Unpacks a single molecule, which can move around in space."
  [dihedral-angles v]
  (let [[dihedral-angles translation-part rotation-part] 
	(partition-using [dihedral-angles 3 3] v)]
    [dihedral-angles
     (unpack-point translation-part)
     (unpack-point rotation-part)]))
     
(defn move-molecule [translation rotation center molecule]
  (apply-matrix-to-molecule
   (matrix-product 
    (translation-matrix translation)
    (translation-matrix center)
    (rotation-matrix rotation)
    (translation-matrix (neg center)))
   molecule))
    

; Creating the conformation function
(defn conformation-fn [stationary-molecule molecules]
  (let [rotation-trees (map calculate-rotation-tree 
			    (cons stationary-molecule molecules))
	dihedral-angles (reduce + (map :degrees-of-freedom rotation-trees))
	; Create ranges as a function call?
	dihedral-angle-ranges (repeat dihedral-angles dihedral-angle-range)
	translation-ranges-seq (apply concat (map translation-ranges molecules))
	rotation-ranges-seq (apply concat (repeat (count molecules) rotation-ranges))
	ranges (concat dihedral-angle-ranges 
		       translation-ranges-seq
		       rotation-ranges-seq )
	centers (map center-of-mass molecules)
	conformations (fn [v]
			(let [[dihedrals 
			       translations 
			       rotations] (unpack-molecules 
					   (map :degrees-of-freedom rotation-trees) v)
			       configurations (map molecule-configuration 
						   rotation-trees dihedrals)
			       moved-molecules (map move-molecule 
						   translations
						   rotations
						   centers
						   (rest configurations))]
			  (cons (first configurations) moved-molecules)))]
    {:ranges ranges, :conformations conformations}))

; Conformation function for only one molecule
(defn single-molecule-conformation-fn [molecule]
  (let [rotation-tree (calculate-rotation-tree molecule)
	dihedral-angles (:degrees-of-freedom rotation-tree)
	dihedral-angle-ranges (repeat dihedral-angles dihedral-angle-range)
	translation-ranges-seq (translation-ranges molecule)
	center (center-of-mass molecule)
	conformation (fn [v]
		       (let [[dihedrals translation rotation] 
			     (unpack-molecule dihedral-angles v)]
			 (move-molecule translation rotation center
					(molecule-configuration rotation-tree
								dihedrals))))]
    {:ranges (concat dihedral-angle-ranges 
		     (translation-ranges molecule)
		     rotation-ranges)
     :conformation conformation}))

; Conformation generating function which can also be used for alternating aligner
; Might change the name
(defn alternating-aligner-conformation-fn [stationary-molecule molecules]
  (let [rotation-trees (map calculate-rotation-tree 
			    (cons stationary-molecule molecules))
	dihedral-angles (reduce + (map :degrees-of-freedom rotation-trees))
	dihedral-angle-ranges (repeat dihedral-angles dihedral-angle-range)
	translation-ranges-seq (apply concat (map translation-ranges molecules))
	rotation-ranges-seq (apply concat (repeat (count molecules) rotation-ranges))
	ranges (concat dihedral-angle-ranges 
		       translation-ranges-seq
		       rotation-ranges-seq )
	centers (map center-of-mass molecules)
	conformations (fn [v]
			(let [[dihedrals 
			       translations 
			       rotations] (unpack-molecules 
					   (map :degrees-of-freedom rotation-trees) v)
			       configurations (map molecule-configuration 
						   rotation-trees dihedrals)
			       moved-molecules (map move-molecule 
						   translations
						   rotations
						   centers
						   (rest configurations))]
			  (cons (first configurations) moved-molecules)))
	sub-conformations-fn (fn [i initial-vector]
			       (assert (<= 1 i (count molecules)))
			       (let [translation-offset (+ dihedral-angles 
							   (* (dec i) 3))
				     rotation-offset (+ dihedral-angles
							(* (count molecules) 3)
							(* (dec i) 3))
				     indexes (concat
					      (range translation-offset 
						     (+ 3 translation-offset))
					      (range rotation-offset
						     (+ 3 rotation-offset)))
				     full-vector (fn [v]
						   (apply 
						    assoc 
						    (vec initial-vector)
						    (flatten 
						     (map vector indexes v))))]
				 {:sub-ranges (map (fn [v] [(- v 0.01) (+ v 0.01)])
						   (map (vec initial-vector) indexes))
				  :full-vector full-vector
				  :sub-conformations (comp conformations 
							   full-vector)}))]
  {:ranges ranges
   :conformations conformations
   :sub-conformations-fn sub-conformations-fn}))