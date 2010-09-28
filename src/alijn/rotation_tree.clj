(ns alijn.rotation-tree
  (:import javax.vecmath.Point3d
	  Jama.Matrix
	  [org.openscience.cdk Atom Molecule]
	  org.openscience.cdk.smiles.smarts.SMARTSQueryTool)  
  (:use [alijn math molecule-manipulation]
	clojure.set))

;;; Data strutures

(defstruct rotation-node
  :parent-atom-id
  :entry-atom-id
  :children
  :atom-ids)

(defstruct rotation-tree
  :root-node
  :degrees-of-freedom
  :molecule)

;;; Generating the rotation tree
(defn atom-closest-to-center [molecule]
  (let [atoms (.atoms molecule)
	center (vec-center (map #(.getPoint3d %) atoms))
	center-distance #(distance center (.getPoint3d %))]
    (apply min-key center-distance atoms)))

(def smarts-rotatable-bond-tool (SMARTSQueryTool. "[!$(*#*)&!D1]-!@[!$(*#*)&!D1]"))
(defn rotatable-bonds [molecule]
  (set
   (when (.matches smarts-rotatable-bond-tool molecule)
     (for [[a-1 a-2] (.getUniqueMatchingAtoms smarts-rotatable-bond-tool)]
       (.getBond molecule (.getAtom molecule a-1) (.getAtom molecule a-2))))))

(defn connected-atoms
  [molecule splitting-bond start-atom]
  (loop [result #{start-atom}
	 queue [start-atom]]
    (if (empty? queue)
      result
      (let [current-atom (first queue)
	    bonds (.getConnectedBondsList molecule current-atom)
	    legal-bonds (filter (comp not splitting-bond) bonds)
	    neighbours (set (map #(.getConnectedAtom % current-atom) legal-bonds))
	    unvisited-neighbours (difference neighbours result)
	    result (union result unvisited-neighbours)
	    queue (concat queue (vec unvisited-neighbours))]
	(recur result (rest queue))))))

(defn color-atoms [molecule splitting-bond]
  (loop [colors {}, i 0]
    (let [uncolored-atoms (difference (set (.atoms molecule)) 
				      (set (keys colors)))]
      (if (empty? uncolored-atoms)
	colors
	(let [start-atom (first uncolored-atoms)
	      same-group (connected-atoms molecule splitting-bond start-atom)
	      colors (apply merge colors (for [a same-group] {a i}))]
	  (recur colors (inc i)))))))

(defn build-tree-structure [molecule splitting-bonds atom-colors root-atom group-atom]
  (let [group-atoms (for [a (.atoms molecule) :when (= (atom-colors group-atom) (atom-colors a))] a)
	group-atoms-ids (map #(.getAtomNumber molecule %) group-atoms)
	group-bonds (set (apply concat (for [a group-atoms] (.getConnectedBondsList molecule a))))
	attached-outgoing (intersection splitting-bonds group-bonds)
	splitting-bonds (difference splitting-bonds attached-outgoing)
	children (for [outgoing-bond attached-outgoing
		       :let [bond-atoms (.atoms outgoing-bond)
			     [child-root] (filter (set group-atoms) bond-atoms)
			     other-atom (.getConnectedAtom outgoing-bond child-root)]] 
		   (build-tree-structure molecule splitting-bonds atom-colors 
					 child-root other-atom))]
    (struct rotation-node
	    (.getAtomNumber molecule root-atom)
	    (.getAtomNumber molecule group-atom)
	    children
	    group-atoms-ids)))

(defn count-children [node]
  (+ (count (:children node)) (apply + (map count-children (:children node)))))

(defn calculate-rotation-tree 
  "Main function for generating rotation tree. The returned rotation tree
  can also be used on (shifted and rotated) clones of the molecule."
  [molecule]
  (let [splitting-bonds (rotatable-bonds molecule)
	atom-colors (color-atoms molecule splitting-bonds)
	root-atom (atom-closest-to-center molecule)
	root-node (build-tree-structure molecule splitting-bonds atom-colors 
					root-atom root-atom)
	degrees-of-freedom (count-children root-node)]
    (struct rotation-tree
	    root-node
	    degrees-of-freedom	    
	    molecule)))

;;; Generating a molecule configuration
(defn get-rotation-axis [molecule matrix from-atom-id to-atom-id]
  (normalised
   (vec-sub (move-and-translate-point matrix (.getPoint3d (.getAtom molecule to-atom-id)))
	    (.getPoint3d (.getAtom molecule from-atom-id)))))

(defn visit-child! [node [angle & configuration] molecule accumulated-matrix]
  (let [accumulated-matrix 
	(if (= (:parent-atom-id node) (:entry-atom-id node))
	  accumulated-matrix
	  (let [root-point (.getPoint3d (.getAtom molecule (:parent-atom-id node)))
		start-point (translation-matrix root-point)
		minus-start-point (translation-matrix (neg root-point))
		rotation-axis (get-rotation-axis molecule accumulated-matrix
						 (:parent-atom-id node) (:entry-atom-id node))
		local-transformation (.times
				      start-point
				      (.times
				       (rotation-matrix angle rotation-axis)
				       minus-start-point))]
	    (.times local-transformation accumulated-matrix)))]
    (doseq [id (:atom-ids node) :let [a (.getAtom molecule id)]] 
      (apply-matrix-to-atom! accumulated-matrix a))
    (doall
     (loop [children (:children node)
	    configuration configuration]
       (if (seq children)
	 (let [configuration (visit-child! (first children) configuration molecule accumulated-matrix)]
	   (recur (rest children) configuration))
	 configuration)))))

(defn molecule-configuration 
  "Returns a clone of the molecule from rotation-tree
  where atoms have been rotated according to the 
  configuration. The number of angles in configuration
  must match the nummer of nodes in the rotation-tree." 
  [rotation-tree configuration]
  (let [cloned-molecule (.clone (:molecule rotation-tree))
	initial-matrix (Matrix/identity 4 4)]
    (visit-child! (:root-node rotation-tree) 
		  (cons 0 configuration) 
		  cloned-molecule
		  initial-matrix)
    cloned-molecule))
