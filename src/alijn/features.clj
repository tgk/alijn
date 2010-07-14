(ns alijn.features
  (:use [clj-todo.todo]
	[alijn combinatorics math]
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
       (apply concat)
       (apply hash-map)))

(todo 
 "This function generates a new query-tool every time.
Can they be cached in a nice manner?"

(defn find-feature
  "Result is a coll of atom collections, even though some features
only match a single atom."
  [smarts-string molecule]
  (let [query-tool (SMARTSQueryTool. smarts-string)
	status (.matches query-tool molecule)]
    (if status
      (map (fn [indices] 
	     (map #(.getAtom molecule %) indices))
	   (.getUniqueMatchingAtoms query-tool))
      [])))
)

(defn get-center [& atoms]
  "Returns the center of the atoms as a Point3d."
  (let [points (map #(.getPoint3d %) atoms)]
    (vec-center points)))

(defn feature-groups
  "Extract the features defined in a {name smarts-string} map
from the molecule. Returns collection of {:name :centers}.
The centers are Point3d objects."
  [features molecule]
  (apply
   merge
   (map (fn [[name smarts-string]]
	  (let [groups (find-feature smarts-string molecule)
		centers (map (partial apply get-center) groups)]
	    {name centers}))
	features)))
