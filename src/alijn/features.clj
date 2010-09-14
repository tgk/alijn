(ns alijn.features
  (:use [alijn combinatorics math utils io]
	[clojure pprint set])
  (:require [clojure.contrib.str-utils2 :as str-utils])
  (:import [org.openscience.cdk.smiles.smarts SMARTSQueryTool]))

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
       (group-by :comment)
       (map-on-values (partial group-by :last-tag))
       (map-on-values (partial map-on-values (partial map :smarts)))))

;;; Query tools
(defn get-query-tool [smarts-string] (SMARTSQueryTool. smarts-string))
(def cached-query-tool (memoize get-query-tool))
(defn find-feature-atoms
  "Result is a coll of atom collections, even though some features
only match a single atom."
  [molecule smarts-string]
  (let [query-tool (cached-query-tool smarts-string)
	status (.matches query-tool molecule)]
    (when status
      (for [indices (.getUniqueMatchingAtoms query-tool)]
	(map #(.getAtom molecule %) indices)))))

(defn get-center [atoms]
  "Returns the center of the atoms as a Point3d."
  (let [points (map #(.getPoint3d %) atoms)] ; memfn?
    (vec-center points)))

(defn new-find-feature
  [molecule feature]
  (set
   (for [found-atoms (find-feature-atoms molecule (:smarts feature))]
     (get-center
      (if (= :all (:points feature))
	found-atoms
	(map (partial nth found-atoms) (:points feature)))))))

(defn new-find-features
  "features is a seq of {:smarts <text> :points <int seq>}.
  Returns set of Point3ds corresponding to found features using
  the smarts strings and points to identify important atoms."
  [molecule features]
  (apply union (map (partial new-find-feature molecule) features)))

(defn new-find-all-features 
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