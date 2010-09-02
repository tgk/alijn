(ns alijn.features
  (:use [alijn combinatorics math utils io]
	[clojure pprint set])
  (:require [clojure.contrib.str-utils2 :as str-utils])
  (:import [org.openscience.cdk.smiles.smarts SMARTSQueryTool]))

;;; Parsers

(defn- not-commented? [s]
  (not= \; (first s)))

(defn parse-features
  [filename]
  (->> filename 
       slurp 
       str-utils/split-lines
       (filter not-commented?)
       (map #(.split #" " %))
       (map seq)
       (group-by first)
       (map-on-values (partial map second))
       (map-on-values (fn [val] [val []]))))

(comment pprint (parse-features "data/example/phase_supplement.smarts"))

(defn first-word [s]
  (first (.split #" " s)))

(defn parse-smarts-line [line]
  (let [tokens (str-utils/split line #"\s+")
	smarts (first tokens)
	atom-sel-expr (second tokens)
	atom-sel-expr (if (= atom-sel-expr "point") "point(1)" atom-sel-expr)
	indexes (->> atom-sel-expr
		     (re-seq #"\d+")
		     (map #(Integer/parseInt %))
		     (map dec))]
    {:smarts smarts, 
     :points (if (empty? indexes) :all indexes)}))

(defn parse-smarts-block [block]
  (if (= 0 (count block)) 
    [] 
    (map parse-smarts-line 
	 (str-utils/split-lines block))))

(defn block-starting-with [keyword s]
  (let [keyword-idx (.indexOf s (str keyword))
	tail (.substring s (+ keyword-idx 1 (count keyword)))
	end-idx (.indexOf tail "\n#")
	block (if (> end-idx -1) (.substring tail 0 end-idx) tail)]
    (str-utils/trim block)))
  
(defn remove-defaults [smarts-strings]
  (filter #(not (.startsWith (:smarts %) "default")) smarts-strings))

(defn parse-feature-block [block]
  {:identifier (block-starting-with "IDENTIFIER" block)
   :comment (block-starting-with "COMMENT" block)
   :include (remove-defaults 
	     (parse-smarts-block (block-starting-with "INCLUDE" block)))
   :exclude (remove-defaults
	     (parse-smarts-block (block-starting-with "EXCLUDE" block)))})

(defn split-into-phase-blocks [s]
  (map 
   #(apply str (interpose "\n" %))
   (chop-using (partial = "#FEATURE") (str-utils/split-lines s))))

;;; Old code ;;;

(defn parse-phase-block [block]
  (let [name (-> block rest first (.substring 9) (str-utils/replace " " "-"))
	[include exclude] (chop-using #(.equals % "#EXCLUDE") (drop 3 block))
	include (map first-word (remove-defaults include))
	exclude (map first-word (remove-defaults exclude))]
    {name [include exclude]}))

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

(comment pprint (parse-phase-features "data/example/phase_smarts.def"))

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

(defn find-and-exclude-features
  "Finds all the features matching the include SMARTS strings,
but excludes points matching patterns matching the exclude SMARTS strings."
  [molecule [include exclude]]
  (let [included (set (find-features molecule include))
	excluded (set (find-features molecule exclude))]
    (difference included excluded)))
   
(defn feature-groups
  "Extract the features defined in a {name [include exclude-smarts-strings]} map
from the molecule. Returns collection of name -> centers.
The centers are Point3d objects."
  [features molecule]
  (map-on-values (partial find-and-exclude-features molecule) features))

(comment let [features (parse-phase-features "data/example/phase_smarts.def")
      molecules (read-molecules "data/example/comt_subset.sdf")]
  (println)
  (doseq [molecule molecules]
    (pprint
     (feature-groups features molecule))))