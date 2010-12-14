(ns alijn.molecule-utils
  (:use [alijn math graph])
  (:import org.openscience.cdk.isomorphism.AtomMappingTools
	   [org.openscience.cdk.interfaces IAtom IMolecule]
	   java.util.HashMap))

(defn molecule-name [molecule] (.get (.getProperties molecule) "cdk:Title"))

(defn add-to-molecule-name! [molecule s]
  (let [new-name (str (molecule-name molecule) s)]
    (.setProperty molecule "cdk:Title" new-name)))

(defn molecule-rmsd
  "Calculates the root mean square deviation between two
  molecules atoms. Must have same number of atoms."
  [molecule-1 molecule-2]
  (when 
      (not= (.getAtomCount molecule-1) (.getAtomCount molecule-2))
      (throw (Exception. (format 
			  "Cannot calculate rmsd between %s and %s as they do not have the same number of atoms." 
			  (molecule-name molecule-1)
			  (molecule-name molecule-2)))))
  (let [rmsd
	(rmsd (map #(.getPoint3d %) (.atoms molecule-1))
	      (map #(.getPoint3d %) (.atoms molecule-2)))]
    rmsd))

(defn atom-neighbours [#^IMolecule molecule #^IAtom a]
  (seq (.getConnectedAtomsList molecule a)))

(defn edge-distances
  [#^IMolecule molecule #^IAtom root-atom]
  (node-distances root-atom
		  (partial atom-neighbours molecule)))