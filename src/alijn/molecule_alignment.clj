(ns alijn.molecule-alignment
  (:gen-class)
  (:use clj-todo.todo)
  (:use [alijn features kabsch combinatorics point-alignment io molecule-manipulation]
	[clojure.contrib combinatorics pprint str-utils command-line])
  (:import [javax.vecmath Point3d]))

(defn molecule-name [molecule] (.get (.getProperties molecule) "cdk:Title"))

;;; Functions added in 1.2, delete when updating
(todo
"This function actually is in clojure 1.2, so this can be removed when updating."
(defn group-by
  "Groups the seq by the keys generated from group-fn."
  [group-fn seq]
  (apply merge-with concat (map (fn [elm] {(group-fn elm) [elm]}) seq)))
)

(defn rand-nth [coll]
  (nth coll (rand-int (count coll))))

;;; end of 1.2 functionality

;;; Other helpers
(defn map-on-values
  "Applies f to the values in the map m."
  [f m] (apply merge (map (fn [[k v]] {k (f v)}) m)))

;;; Pre alignment data massage
(defn add-name-and-features
  [feature-definitions conformation]
  {:name (molecule-name conformation) 
   :conformation conformation
   :features (feature-groups feature-definitions conformation)})

;;; Aligner
(defn all-alignments-over-selected-conformations
  [conformation-selector grouped-conformations]
  (let [names (keys grouped-conformations)
	conformations-list (map grouped-conformations names)]
    (map 
     (fn [combination]
       (let [named-feature-combination (->> combination (map :features) (zipmap names))
	     alignment (optimal-alignment-over-all-groups named-feature-combination)]
	 (assoc alignment 
	   :conformations (->> combination (map :conformation) (zipmap names)))))
     (conformation-selector conformations-list))))

;;; Conformation selectors
(defn all-conformations-selector [conformations-list]
  (apply cartesian-product conformations-list))

(defn sample-conformations-selector [samples]
  (fn [conformations-list]
    (map (fn [_] (map rand-nth conformations-list)) (range samples))))

;;; Wrapped aligners
(defn optimal-alignment-over-selected-conformations
  [conformation-selector grouped-conformations]
  (smallest-alignment-rmsd-sum
   (all-alignments-over-selected-conformations 
    conformation-selector grouped-conformations)))

(def optimal-alignment-over-all-conformations
     (partial optimal-alignment-over-selected-conformations all-conformations-selector))

(defn optimal-alignment-over-sampled-conformations [samples]
  (partial optimal-alignment-over-selected-conformations
	   (sample-conformations-selector samples)))

;;; Post alignment molecule movement
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

;;; Controller
(defn extract-features-and-align
  [aligner conformations-filename feature-definitions]
  (->> conformations-filename
       read-sdf-file
       (map (partial add-name-and-features feature-definitions))
       (group-by :name)
       aligner))

;;; Main method
(defn perform-alignment 
  "Methods written to only take strings as to be easy to call from lein run."
  [& args]
  (comment println "Extracting and aligning features")
  (with-command-line
    args
    "Program for aligning sdf files of conformations according to features and
outputting the result to a sdf file."
    [[samples s  "Number of samples to use"]
     [all? a?    "Iterate over all pairs of combinations, don't sample"]
     [output o   "Output sdf file"]
     [input i    "Input sdf file"]
     [features f "Feature file of smarts strings"]]

    (let [aligner (cond 
		   samples (optimal-alignment-over-sampled-conformations 
			    (Integer/parseInt samples))
		   all? optimal-alignment-over-all-conformations)
	  features (parse-features features)
	  optimal-alignment (extract-features-and-align
			     aligner input features)
	  no-solution? (contains? (map :no-solution 
				       (vals (:alignment optimal-alignment))) 
				  true)]
   (if no-solution?
      (do
	(println "No solution was found.")
	(println "This is because at least one molecule didn't have any common features to any of the other molecules."))
      (do
	(println "Optimal alignment has reference " (:reference-name optimal-alignment))
	(println "Reference points are")
	(pprint (:reference-features optimal-alignment)) 
	(println ":alignment field is")
	(pprint (:alignment optimal-alignment))
	(println "Other keys from result:" (keys optimal-alignment))
	(println "Writing moved conformations to file" output)
	(write-sdf-file 
	 output (move-molecules-from-alignment optimal-alignment)))))))

(def -main perform-alignment)

(comment -main 
	 "-s" "7" 
	 "-o" "test42.sdf" 
	 "-i" "data/debug/carboxy.sdf" 
	 "-f" "data/debug/features.smarts")