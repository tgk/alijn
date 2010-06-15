(ns alijn.molecule-alignment
  (:use clj-todo.todo)
  (:use [alijn pharmacophore kabsch combinatorics point-alignment])
  (:import [javax.vecmath Point3d])
  (:import 
   [java.io File FileInputStream]
   [org.openscience.cdk DefaultChemObjectBuilder]
   [org.openscience.cdk.io.iterator IteratingMDLReader]))

; Bottom up code

(defn kabsch-on-pairing 
  [pairing]
  (let [pharm-names (map first pairing)
	points (map second pairing)
	ref-points (center-points (map first points))
	subj-points (center-points (map second points))
	subj-rotated (kabsch ref-points subj-points)
	deviation (rmsd ref-points subj-rotated)
	result (map vector pharm-names subj-rotated)]
    {:result result
     :rmsd deviation}))


; Top down code

(defn read-sdf-file [filename]
  (let [file (File. filename)
        stream (FileInputStream. file)
        reader 
          (IteratingMDLReader. stream (DefaultChemObjectBuilder/getInstance))]
    (iterator-seq reader)))
	
(defn group-by
  "Groups the seq by the keys generated from group-fn."
  [group-fn seq]
  (apply
   merge-with
   concat
   (map (fn [elm] {(group-fn elm) [elm]}) seq)))

(defn group-conformations-by-name
  [conformations]
  (group-by (fn [molecule] (.getName molecule)) conformations))

(defn map-on-values
  "Applies f to the values in the map m."
  [f m]
  (apply merge (map (fn [[k v]] {k (f v)}) m)))

(defn generate-pharmacophores
  [grouped-conformations]
  (map-on-values
   (fn [conformations] (map find-pharmacophore conformations))
   grouped-conformations))

