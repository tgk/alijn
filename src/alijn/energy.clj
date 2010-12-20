(ns alijn.energy
  (:import org.openscience.cdk.charges.MMFF94PartialCharges)
  (:import [org.openscience.cdk.interfaces IAtom IAtomContainer]
	   javax.vecmath.Point3d)
  (:use clojure.set
	clojure.contrib.profile
	alijn.molecule-utils))

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
(defn atom-to-atom-mapping [molecule-1 molecule-2]
  (zipmap (.atoms molecule-1) (.atoms molecule-2)))

(defn replace-keys-and-vals [m replace]
  (into {} (for [[k vs] m] [(replace k) (map replace vs)])))

(defn replace-keys [m replace]
  (into {} (for [[k v] m] [(replace k) v])))

(defn linear-punishment [d]
  (let [cutoff 3.3] ; Aangstroem
    (if (> d cutoff) 
      0
      (- cutoff d))))

(defn almost-linear-punishment [d]
  (let [cutoff 1.5] ; Aangstroem
    (if (> d cutoff) 0 (+ 10 (- cutoff d)))))

(defn steric-clash-energy [#^IAtomContainer molecule]
  (prof
   :steric-clash
   (reduce
    +
    (flatten
     (for [#^IAtom a (.atoms molecule)
	   :let [#^Point3d p (.getPoint3d a)
		 dist (prof :edge-distance (edge-distances molecule a))]]
       (for [#^IAtom b (.atoms molecule)
	     :when (> (dist b) 2)
	     :let [#^Point3d q (.getPoint3d b)]]
	 (almost-linear-punishment (.distance p q))))))))

