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

(defn optimal-alignment-over-all-conformations
  [conformations-pharmacophores]
  (select-optimal
   (all-alignments-over-all-conformations
    conformations-pharmacophores)))

; Test by printing

(defn -main [& args]
  (def filename "data/debug/g-phosphorylase-standard.sdf")
  (def conformations (read-sdf-file filename))
  (println filename)
  
  (def grouped (group-conformations-by-name conformations))
  (println (map-on-values count grouped))

  (def pharmacophore-definitions example-pharmacophores)
  (def pharmacophores (generate-pharmacophores pharmacophore-definitions grouped))

  (println)
  (println "The first")
  (def first-conf-pharm (map-on-values first pharmacophores))
  (println first-conf-pharm)

  (comment
  (def first-optimal (optimal-alignment-over-all-groups first-conf-pharm))
  (println)
  (println "Optimal of first entry")
  (println first-optimal)
  )

  (comment
  (println)
  (println "All alignments over all conformations")
  (def all-over-conf (all-alignments-over-all-conformations pharmacophores))
  (println all-over-conf)
  )

  (println)
  (println "Optimal! Yeah! The goal!")
  (def optimal (optimal-alignment-over-all-conformations pharmacophores))
  (println optimal)
  (println "Done!")
)

(-main)

