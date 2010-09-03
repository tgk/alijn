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

;;; Phase file parsing

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

(defn is-tag? [line] (.startsWith line "#"))
(defn tag-to-attribute [tag] (-> tag (.substring 1) .toLowerCase keyword))
(defn read-tag [line] 
  (let [[tag & rest] (.split line " ")]
    [(tag-to-attribute tag) (apply str (interpose " " rest))]))

(defn update-tag-info [line tag-info]
  (if (is-tag? line)
    (let [[tag val] (read-tag line)]
      (assoc tag-info 
	tag val
	:last-tag tag
	:tag? true))
    (assoc tag-info :tag? false)))

(defn tag-lines [lines]
  (loop [in-lines lines, out-lines [], tag-info {:tags {}}]
    (if (empty? in-lines)
      out-lines
      (let [[in-line & in-lines] in-lines
	    tag-info (update-tag-info in-line tag-info)
	    out-line (assoc tag-info :line in-line)]
	(recur in-lines, (conj out-lines out-line), tag-info)))))

(defn parse-phase-with-tags [filename]
  (->> filename
       slurp
       str-utils/split-lines
       (filter #(not (.startsWith % "default")))
       tag-lines
       (filter (comp not :tag?))
       (map #(assoc % :smarts (parse-smarts-line (:line %))))
       (group-by :identifier)
       (map-on-values (partial group-by :last-tag))
       (map-on-values (partial map-on-values (partial map :smarts)))))

;;; Query tools

; Save
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

; Phase query tools
(defn new-find-features
  "features is a seq of {:smarts <text> :points <int seq>}.
  Returns set of Point3ds corresponding to found features using
  the smarts strings and points to identify important atoms."
  [molecule features]

;;;;;; TODO : Here is as far as I've got

)

(defn new-find-features 
  "Extracts all the feature points from the molecule using
  the feature-defs which are organised as 
  {name {:include include, :exclude exclude}.
  Returned features are maps from name to a seq of Point3ds."
  [molecule feature-defs]
  (map-on-values  
   (fn [{include :include, exclude :exclude}]
     (difference (new-find-features molecule include)
		 (new-find-features molecule exclude)))
   feature-defs))

; Remove
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
