(ns alijn.features
  (:use [alijn combinatorics math utils io]
	[clojure.contrib pprint])
  (:require [clojure.contrib.str-utils2 :rename str-utils])
  (:import [org.openscience.cdk.smiles.smarts SMARTSQueryTool]))

;;; Parsers

(defn- not-commented? [s]
  (not (= \; (first s))))

(defn parse-features
  [filename]
  (->> filename 
       slurp 
       str-utils/split-lines
       (filter not-commented?)
       (map #(.split #" " %))
       (map seq)
       (group-by first)
       (map-on-values (partial map second))))

(def example-phase-block
      (list 
       "#IDENTIFIER D"
       "#COMMENT Donor (D)"
       "#INCLUDE "
       "[#1][O;X2]                              vector(1)   0   1   -1   0   1   1.8"
       "[#1]S[#6]                               vector(1)   0   1   -1   0   1   1.8"
       "[#1][C;X2]#[C;X2]                       vector(1)   0   1   -1   0   1   1.8"
       "[#1][NX3]C(=[NX2])[#6]                  vector(1)   0   1   -1   0   1   1.8"
       "[#1][#7]                                vector(1)   0   1   -1   0   1   1.8"
       "#EXCLUDE"
       "[#1]OC(=O)                              point(1)   0   1   0   0   1   1.8"
       "[#1]O[S;X3]=O                           point(1)   0   1   0   0   1   1.8"
       "[#1]O[S;X4](=O)(=O)                     point(1)   0   1   0   0   1   1.8"
       "[#1]O[P;X3]=O                           point(1)   0   1   0   0   1   1.8"
       "[#1]O[P;X4]=O                           point(1)   0   1   0   0   1   1.8"
       "[#1]n1nnnc1                             point(1)   0   1   0   0   1   1.8"
       "[#1]N([S;X4](=O)(=O))(C(F)(F)(F))       point(1)   0   1   0   0   1   1.8"
       "[#1]([NH2;X3,NH3]([#6;X4]))             point(1)   0   1   0   0   1   1.8"
       "[#1]([NH;X3,NH2]([#6;X4])([#6;X4]))     point(1)   0   1   0   0   1   1.8"
       "[#1]([NH;X4]([#6;X4])([#6;X4])([#6;X4]))   point(1)   0   1   0   0   1   1.8"
       "[#1][NX3]C(=[NX2])[NX3]                 point(1)   0   1   0   0   1   1.8"
       "[#1][NX3]C(=[NX3+])                     point(1)   0   1   0   0   1   1.8"
       "[#1][NX3+]=C[NH2]                       point(1)   0   1   0   0   1   1.8"
       "[#1][NX3]C(=[NX2])                      point(1)   0   1   0   0   1   1.8"
       "[#1][NX3][#6](=[NX2,NX3+])[#6]          point(1)   0   1   0   0   1   1.8"))

(defn parse-phase-block [block]
  (let [name (-> block rest first (.substring 9) (str-utils/replace " " "-"))
	included (take-while #(not (.equals % "#EXCLUDE")) (drop 3 block))
	not-default (filter #(not (.startsWith % "default")) included)]
    {name 
     (map first (map #(.split #" " %) not-default))}))

(defn parse-phase-features
  "Parse a phase-type feature file. Ignores default lines (non-SMARTS strings)
and Custom patterns."
  [filename]
  (dissoc 
   (->> filename
	slurp
	str-utils/split-lines
	(chop-using #(.equals % "#FEATURE"))
	rest
	(map parse-phase-block)
	(apply merge))
   "Custom"))

;;; Query tools

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