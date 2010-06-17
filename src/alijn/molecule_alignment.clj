(ns alijn.molecule-alignment
  (:gen-class)
  (:use clj-todo.todo)
  (:use [alijn pharmacophore kabsch combinatorics point-alignment]
	[clojure.contrib combinatorics])
  (:import [javax.vecmath Point3d])
  (:import 
   [java.io File FileInputStream]
   [org.openscience.cdk DefaultChemObjectBuilder]
   [org.openscience.cdk.io.iterator IteratingMDLReader]))

(defn read-sdf-file [filename]
  (let [file (File. filename)
        stream (FileInputStream. file)
        reader 
          (IteratingMDLReader. stream (DefaultChemObjectBuilder/getInstance))]
    (iterator-seq reader)))

(defn molecule-name [molecule]
  (.get (.getProperties molecule) "cdk:Title"))
	
(defn group-by
  "Groups the seq by the keys generated from group-fn."
  [group-fn seq]
  (apply
   merge-with
   concat
   (map (fn [elm] {(group-fn elm) [elm]}) seq)))

(defn group-conformations-by-name
  [conformations]
  (group-by molecule-name conformations))

(defn map-on-values
  "Applies f to the values in the map m."
  [f m]
  (apply merge (map (fn [[k v]] {k (f v)}) m)))

(defn generate-pharmacophores
  [pharmacophore-definitions grouped-conformations]
  (map-on-values
   (fn [conformations] 
     (map 
      (partial pharmacophore-groups pharmacophore-definitions) 
      conformations))
   grouped-conformations))

(todo
 "Is this yet another thing that should be in alijn.point-alignment?"
(defn all-alignments-over-all-conformations
  [conformations-pharmacophores]
  (let [conf-names (keys conformations-pharmacophores)
	confs (map conformations-pharmacophores conf-names)
	combinations (apply cartesian-product confs)]
    (map 
     (fn [combi]
       (let [named-combi (zipmap conf-names combi)]
	   (optimal-alignment-over-all-groups named-combi)))
     combinations)))
)

(todo
 "And this, should this also be in alijn.point-alignment?"
(defn optimal-alignment-over-all-conformations
  [conformations-pharmacophores]
  (select-optimal
   (all-alignments-over-all-conformations
    conformations-pharmacophores)))
)

; Test by printing, bad! :-s

(defn extract-pharmacophores-and-align
  [conformations-filename pharmacophore-definitions]
  (->> conformations-filename
       read-sdf-file
       group-conformations-by-name
       (generate-pharmacophores pharmacophore-definitions)
       optimal-alignment-over-all-conformations))

(defn -main [& args
	     ;pharmacophore-definitions-filename
	     ;conformations-filename
	     ]
  (println args)
  (comment println "Extracting and aligning pharmacophores")

  (comment def pharmacophores example-pharmacophores)

  (comment println (extract-pharmacophores-and-align
	    conformations-filename
	    pharmacophores)))

(defn perform-alignment [pharmacophore-definitions-filename
			 conformations-filename]
  (println "Extracting and aligning pharmacophores")

  (def pharmacophores (parse-pharmacophores pharmacophore-definitions-filename))

  (println (extract-pharmacophores-and-align
	    conformations-filename
	    pharmacophores)))
