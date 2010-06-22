(ns alijn.molecule-alignment
  (:gen-class)
  (:use clj-todo.todo)
  (:use [alijn features kabsch combinatorics point-alignment io molecule-manipulation]
	[clojure.contrib combinatorics pprint])
  (:import [javax.vecmath Point3d]))

(defn molecule-name [molecule] (.get (.getProperties molecule) "cdk:Title"))

(defn group-by
  "Groups the seq by the keys generated from group-fn."
  [group-fn seq]
  (apply merge-with concat (map (fn [elm] {(group-fn elm) [elm]}) seq)))

(defn map-on-values
  "Applies f to the values in the map m."
  [f m] (apply merge (map (fn [[k v]] {k (f v)}) m)))

(todo
 "Wait! Is this method throwing away all the pairwise computations?
Gotta memorise the other one, whatever optimal-alignment-over-all-groups calls.
Check to see what the difference in performance is."
(defn all-alignments-over-all-conformations
  [grouped-conformations]
  (let [names (keys grouped-conformations)
	combinations (apply cartesian-product (map grouped-conformations names))]
    (map 
     (fn [combination]
       (let [named-feature-combination (->> combination (map :features) (zipmap names))
	     alignment (optimal-alignment-over-all-groups named-feature-combination)]
	 (assoc alignment 
	   :combination (->> combination (map :conformation) (zipmap names)))))
     combinations)))
)

(todo
 "And this, should this also be in alijn.point-alignment?"
(defn optimal-alignment-over-all-conformations
  [conformations-and-features]
  (select-optimal
   (all-alignments-over-all-conformations
    conformations-and-features)))
)

; Test by printing, bad! :-s

(defn extract-features-and-align
  [conformations-filename feature-definitions]
  (->> conformations-filename
       read-sdf-file
       (map 
	(todo "Might move this function out"
	(fn [conformation] 
	  {:name (molecule-name conformation) 
	   :conformation conformation
	   :features (feature-groups feature-definitions conformation)})))
       (group-by :name)
       optimal-alignment-over-all-conformations))

(defn perform-alignment [feature-definitions-filename
			 conformations-filename]
  (println "Extracting and aligning features")
  (def features (parse-features feature-definitions-filename))
  (pprint (extract-features-and-align
	   conformations-filename
	   features)))