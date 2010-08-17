(ns alijn.point-alignment
  (:use [clj-todo]
	[alijn combinatorics kabsch utils]
	[clojure.contrib pprint])
  (:import [javax.vecmath Point3d]))

;;; Shiny new code (some untested)

(def all-alignments-on-labelled-pairings
 (memoize
  (fn
    [reference labelled-points]
    (let [labels (keys reference)
	  grouped-points (all-grouped-pairs (map reference labels) (map labelled-points labels))
	  flat-grouped-labels (map (partial map flatten-groups) grouped-points)]
      (map
       (fn [[[flat-reference _] [flat-labelled-points unflatten]]]
	 (if (and (> (count flat-reference) 0)
		  (> (count flat-labelled-points) 0))
	   (let [result (kabsch-with-translation flat-reference flat-labelled-points)]
	     (assoc result 
	       :rotated-points (->> result :rotated-points unflatten (zipmap labels))))
	   {:rmsd (Double/POSITIVE_INFINITY)
	    :no-solution true})) ; Pretty sure this is the culprit!
       flat-grouped-labels)))))

; Terrible names!
(def alignments-on-groups-pair all-alignments-on-labelled-pairings)

(defn optimal-alignment-on-all
  [reference-groups target-groups-groups]
  (map
   (fn [target-groups]
     (apply min-key :rmsd (alignments-on-groups-pair reference-groups target-groups)))
   target-groups-groups))

;all-alignments-on-conformation-pairings
(defn alignments-over-all-groups
  [group-of-groups]
  (let [elm (keys group-of-groups)]
    (map
     (fn [[reference-group-name target-groups-names]]
       (let [reference-group (group-of-groups reference-group-name)
	     target-groups (map group-of-groups target-groups-names)
	     alignments (optimal-alignment-on-all reference-group target-groups)
	     named-alignments (zipmap target-groups-names alignments)]
	 {:reference-name reference-group-name,
	  :reference-features reference-group,
	  :alignment named-alignments}))
     (leave-one-out elm))))

(defn alignment-rmsd-sum
  [alignment]
  (let [rmsds (->> alignment :alignment vals (map :rmsd))] 
    (reduce + rmsds)))

(defn smallest-alignment-rmsd-sum
  [alignments]
  (apply min-key alignment-rmsd-sum alignments))

; Terrible name
(defn optimal-alignment-over-all-groups
  [group-of-groups]
  (smallest-alignment-rmsd-sum
   (alignments-over-all-groups group-of-groups)))