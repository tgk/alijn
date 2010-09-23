(ns alijn.molecule-utils
  (:use [alijn math])
  (:import org.openscience.cdk.isomorphism.AtomMappingTools
	   java.util.HashMap))

(defn molecule-name [molecule] (.get (.getProperties molecule) "cdk:Title"))

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

(defn molecule-rmsd-different-molecules
  "Calculates the root mean square deviation between two
  molecules that do not have the same number of atoms.
  Uses CDKs AtomMappingTools to match atoms."
  [molecule-1 molecule-2]
  (let [mapping (AtomMappingTools/mapAtomsOfAlignedStructures
		 molecule-1 molecule-2 (HashMap.))
	
	]
    mapping))