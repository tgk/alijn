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
	   :conformations (->> combination (map :conformation) (zipmap names)))))
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

(defn move-molecules-from-alignment
  "Takes an alignment such as the one from opimal-alignment-over-all-conformations 
and returns the moved conformations to align with their optimal features positions. 
The reference molecule is kept still. "
  [alignment]
  (let [names (-> alignment :alignment keys)
	configurations (map (alignment :alignment) names)
	translations (map :translation configurations)
	rotations (map :rotation configurations)
	conformations (map (alignment :conformations) names)
	moved-molecules (map 
			 translate-and-rotate-molecule 
			 translations rotations conformations)
;	named-moved-molecules (zipmap names moved-molecules)
	reference (:reference-name alignment)]
    (cons ((alignment :conformations) reference)
	  moved-molecules
	  )))


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
			 conformations-filename
			 output-filename]
  (println "Extracting and aligning features")
  (def features (parse-features feature-definitions-filename))
  (let [optimal-alignment (extract-features-and-align
			   conformations-filename
			   features)]
    (println "Optimal alignment has reference " (:reference-name optimal-alignment))
    (println "Other keys from result:" (keys optimal-alignment))
    (println "Writing moved conformations to file" output-filename)
    (write-sdf-file 
     output-filename 
     (move-molecules-from-alignment optimal-alignment))))