(ns alijn.features
  (:use [alijn combinatorics math utils io]
	[clojure.contrib.str-utils2 :only [split-lines split]])
  (:import 
   [org.openscience.cdk.smiles.smarts SMARTSQueryTool]))

(defn- not-commented? [s]
  (not (= \; (first s))))

(defn parse-features
  [filename]
  (->> filename 
       slurp 
       split-lines
       (filter not-commented?)
       (map #(.split #" " %))
       (map seq)
       (group-by first)
       (map-on-values (partial map second))))

(defn get-query-tool [smarts-string] (SMARTSQueryTool. smarts-string))
(def cached-query-tool (memoize get-query-tool))
(defn find-feature-atoms
  "Result is a coll of atom collections, even though some features
only match a single atom."
  [molecule smarts-string]
  (let [query-tool (cached-query-tool smarts-string)
	status (.matches query-tool molecule)]
    (if status
      (map (fn [indices] 
	     (map #(.getAtom molecule %) indices))
	   (.getUniqueMatchingAtoms query-tool))
      [])))

(defn get-center [atoms]
  "Returns the center of the atoms as a Point3d."
  (let [points (map #(.getPoint3d %) atoms)]
    (vec-center points)))

(defn find-features
  "Finds all the features with the given SMARTS strings.
Returns the center points."
  [molecule smarts-strings]
  (->>  smarts-strings
	(map (partial find-feature-atoms molecule))
	(apply concat)
	(map get-center)
	distinct))
   
(defn feature-groups
  "Extract the features defined in a {name smarts-strings} map
from the molecule. Returns collection of name -> centers.
The centers are Point3d objects."
  [features molecule]
  (map-on-values (partial find-features molecule) features))

(comment let [features (parse-features "data/example/phase.smarts")
      molecules (read-molecules "data/example/comt_subset.sdf")]
  (println)
  (println features)
  (doseq [molecule molecules]
    (println
     (feature-groups features molecule))))