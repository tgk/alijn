(ns alijn.molecule-alignment
  (:gen-class)
  (:use clj-todo.todo)
  (:use [alijn features kabsch combinatorics point-alignment io molecule-manipulation]
	[clojure.contrib combinatorics pprint])
  (:import [javax.vecmath Point3d]))

(defn molecule-name [molecule] (.get (.getProperties molecule) "cdk:Title"))

(todo
"This function actually is in clojure 1.2, so this can be removed when updating."
(defn group-by
  "Groups the seq by the keys generated from group-fn."
  [group-fn seq]
  (apply merge-with concat (map (fn [elm] {(group-fn elm) [elm]}) seq)))
)

(defn map-on-values
  "Applies f to the values in the map m."
  [f m] (apply merge (map (fn [[k v]] {k (f v)}) m)))

(defn all-alignments-over-all-conformations
  [grouped-conformations]
  (let [names (keys grouped-conformations)
	conformations-list (map grouped-conformations names)]
    (map 
     (fn [combination]
       (let [named-feature-combination (->> combination (map :features) (zipmap names))
	     alignment (optimal-alignment-over-all-groups named-feature-combination)]
	 (assoc alignment 
	   :conformations (->> combination (map :conformation) (zipmap names)))))
     (apply cartesian-product conformations-list))))

(defn optimal-alignment-over-all-conformations
  [conformations-and-features]
  (smallest-alignment-rmsd-sum
   (all-alignments-over-all-conformations
    conformations-and-features)))

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
	reference (:reference-name alignment)]
    (cons ((alignment :conformations) reference)
	  moved-molecules)))

(defn add-name-and-features
  [feature-definitions conformation]
  {:name (molecule-name conformation) 
   :conformation conformation
   :features (feature-groups feature-definitions conformation)})

(defn extract-features-and-align
  [conformations-filename feature-definitions]
  (->> conformations-filename
       read-sdf-file
       (map (partial add-name-and-features feature-definitions))
;       (map #(assoc % :feature-counts (map-on-values count (:features %))))
;       (map #(dissoc % :features :conformation))
       (group-by :name)
;       pprint
       optimal-alignment-over-all-conformations
  ))

(defn perform-alignment [feature-definitions-filename
			 conformations-filename
			 output-filename]
  (println "Extracting and aligning features")
  (def features (parse-features feature-definitions-filename))
  (let [optimal-alignment (extract-features-and-align
			   conformations-filename
			   features)
	no-solution? (contains? (map :no-solution (vals (:alignment optimal-alignment))) true)]
    (if no-solution?
      (do
	(println "No solution was found.")
	(println "This is because at least one molecule didn't have any common features to any of the other molecules."))
      (do
	(println "Optimal alignment has reference " (:reference-name optimal-alignment))
	(println ":alignment field is")
	(pprint (:alignment optimal-alignment))
	(println "Other keys from result:" (keys optimal-alignment))
	(println "Writing moved conformations to file" output-filename)
	(write-sdf-file 
	 output-filename 
	 (move-molecules-from-alignment optimal-alignment))))))