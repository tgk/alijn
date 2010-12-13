(ns alijn.energy
  (:import org.openscience.cdk.charges.MMFF94PartialCharges)
  (:import [org.openscience.cdk.interfaces IAtom IAtomContainer]
	   javax.vecmath.Point3d)
  (:use clojure.set
	clojure.contrib.profile))

; MMFF94 force field

(def charge-calculator (MMFF94PartialCharges.))

(defn total-MMFF94-charges!
  "Calculates the MMFF94 charge of every atom and
  returns the total charge of the molecule.
  Alters molecule by adding the property 'MMFF94charge'
  to every atom."
  [molecule]
  (.calculateCharges charge-calculator molecule)
  (reduce + (map #(.getProperty % "MMFF94charge") (.atoms molecule))))

; Simple linear clash

(defn atom-bond-distances 
  "Returns a map with atom bond distance to every other atom in molecule."
  [molecule a]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY a)
	 distance {a 0}]
    (if (empty? queue)
      distance
      (let [unvisited (difference (set (.getConnectedAtomsList molecule (peek queue)))
				  (set (keys distance)))]
	(recur (into (pop queue) unvisited)
	       (apply merge distance (for [c unvisited] 
				       {c (inc (distance (peek queue)))})))))))

(defn far-away-atoms 
  "Returns all atoms further away than three bonds."
  [molecule a]
  (for [[k dist] (atom-bond-distances molecule a) :when (>= dist 3)] k))  

(defn linear-punishment [d]
  (let [cutoff 3.3] ; Aangstroem
    (if (> d cutoff) 
      0
      (- cutoff d))))

(defn steric-overlap [#^IAtomContainer molecule]
  (prof
   :steric-overlap
   (reduce
    +
    (flatten
     (for [#^IAtom a (.atoms molecule)
	   :let [#^Point3d p (.getPoint3d a)]]
       (for [#^IAtom b (.atoms molecule)
	     :when (not= a b)
	     :let [#^Point3d q (.getPoint3d b)]]
	 (linear-punishment (.distance p q))))))))