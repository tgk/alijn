(ns alijn.rotation-tree
  (:import javax.vecmath.Point3d
	  Jama.Matrix
	  [org.openscience.cdk Atom Molecule]
	  org.openscience.cdk.smiles.smarts.SMARTSQueryTool)  
  (:use alijn.math
	clojure.set))

(defstruct rotation-node
  :root-atom-id
  :rotation-axis
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
	    queue (into queue unvisited-neighbours)]
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

(defn build-tree-structure [molecule splitting-bonds atom-colors root-atom group-atom rotation-axis]
  (let [group-atoms (for [a (.atoms molecule) :when (= (atom-colors group-atom) (atom-colors a))] a)
	group-atoms-ids (map #(.getAtomNumber molecule %) group-atoms)
	group-bonds (set (apply concat (for [a group-atoms] (.getConnectedBondsList molecule a))))
	attached-outgoing (intersection splitting-bonds group-bonds)
	splitting-bonds (difference splitting-bonds attached-outgoing)
	children (for [outgoing-bond attached-outgoing
		       :let [bond-atoms (.atoms outgoing-bond)
			     [child-root] (filter (set group-atoms) bond-atoms)
			     other-atom (.getConnectedAtom outgoing-bond child-root)
			     child-rotation-axis (normalised (vec-sub (.getPoint3d other-atom) 
								      (.getPoint3d child-root)))]] 
		   (build-tree-structure molecule splitting-bonds atom-colors 
					 child-root other-atom child-rotation-axis))]
    (struct rotation-node
	    (.getAtomNumber molecule root-atom)
	    rotation-axis
	    children
	    group-atoms-ids)))

(defn count-children [node]
  (+ (count (:children node)) (apply + (map count-children (:children node)))))

(defn calculate-rotation-tree [molecule]
  (let [splitting-bonds (rotatable-bonds molecule)
	atom-colors (color-atoms molecule splitting-bonds)
	root-atom (atom-closest-to-center molecule)
	root-node (build-tree-structure molecule splitting-bonds atom-colors 
					root-atom root-atom (Point3d. 0 0 0))
	degrees-of-freedom (count-children root-node)]
    (struct rotation-tree
	    root-node
	    degrees-of-freedom	    
	    molecule)))

;;; Helper methods
(defn rotation-matrix 
  "axis is assumed to be normalised."
  [angle axis]
  (let [c (Math/cos angle), s (Math/sin angle), omc (- 1 c) ;one minus c
	x (.x axis), y (.y axis), z (.z axis)
	xs (* x s), ys (* y s), zs (* z s)
	xyomc (* x y omc), xzomc (* x z omc), yzomc (* y z omc)]
    (Matrix. 
     (double-array
      [(+ (* x x omc) c)  (+ xyomc zs)       (- xzomc ys)       0
       (- xyomc zs)       (+ (* y y omc) c)  (+ yzomc xs)       0
       (+ xzomc ys)       (- yzomc xs)       (+ (* z z omc) c)  0
       0                  0                  0                  1])
     4)))

(defn translation-matrix [translation]
  (doto (Matrix/identity 4 4)
    (.set 0 3 (.x translation))
    (.set 1 3 (.y translation))
    (.set 2 3 (.z translation))))

;;; Generating a molecule configuration
(defn move-and-translate [matrix point]
  (let [point-matrix (doto (Matrix. 4 1)
		       (.set 0 0 (.x point))
		       (.set 1 0 (.y point))
		       (.set 2 0 (.z point))
		       (.set 3 0 1.0))
	moved-matrix (.times matrix point-matrix)]
    (Point3d. (.get moved-matrix 0 0)
	      (.get moved-matrix 1 0)
	      (.get moved-matrix 2 0))))

(defn visit-child! [node [angle & configuration] molecule accumulated-matrix]
  (let [root-point (.getPoint3d (.getAtom molecule (:root-atom-id node)))
	start-point (translation-matrix root-point)
	minus-start-point (translation-matrix (neg root-point))
	local-transformation (.times
			      start-point
			      (.times
			       (rotation-matrix angle (:rotation-axis node))
			       minus-start-point))
	accumulated-matrix (.times local-transformation accumulated-matrix)]
    (doseq [id (:atom-ids node)
	    :let [a (.getAtom molecule id)
		  p (.getPoint3d a)]]
      (.setPoint3d a (move-and-translate accumulated-matrix p)))
    (doseq [child (:children node)]
      (visit-child! child configuration molecule accumulated-matrix))))

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
