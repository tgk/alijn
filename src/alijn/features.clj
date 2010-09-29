(ns alijn.features
  (:import [org.openscience.cdk Atom Bond AtomContainer]
	   [org.openscience.cdk.interfaces IAtom]
	   [org.openscience.cdk.smiles.smarts SMARTSQueryTool])
  (:use alijn.math 
	clojure.pprint
	clojure.contrib.generic.functor
	clojure.contrib.def))

;; Hydrogen donor and acceptor

(defn is-atom? 
  "(is-atom? \"h\")  returns a fn that can be applied to atoms
   (is-atom? atm \"h\")  generates function and applies
   (is-atom? atm \"o\" \"n\")  generates functions and returns if 
                               any symbol is matched"
  ([symbol] (fn [atm] (= symbol (.getSymbol atm))))
  ([atm & symbols] (true? (some #((is-atom? %) atm) symbols))))

(defn connected-to-hydrogen? [container atm] 
  (true? (some (is-atom? "H") (.getConnectedAtomsList container atm))))

(defn is-both-donor-and-acceptor? [container atm]
  (and (is-atom? atm "O") (connected-to-hydrogen? container atm)))

(defn is-donor? [container atm]
  (and (is-atom? atm "S" "N") (connected-to-hydrogen? container atm)))

(defn is-acceptor? [container atm]
  (and (is-atom? atm "O" "N") (not (connected-to-hydrogen? container atm))))

;; Aromatic rings

(def  aromatic-tools (map #(SMARTSQueryTool. %) ["a1aaaa1" "a1aaaaa1" "a1aaaaaa1"]))
(def aliphatic-tools (map #(SMARTSQueryTool. %) ["A1AAAA1" "A1AAAAA1" "A1AAAAAA1"]))

(defn find-rings [tools container]
  (apply 
   concat
   (for [tool tools]
     (when (.matches tool container)
       (for [indices (.getUniqueMatchingAtoms tool)]
	 (map #(.getAtom container %) indices))))))

(defn ring-center [ring-atoms]
  (let [points (map #(.getPoint3d %) ring-atoms)]
    (vec-center points)))

;; Charges

(defn is-positive? [charge-limit atom]
  (>= (.getCharge atom) charge-limit))

(defn is-negative? [charge-limit atom]
  (<= (.getCharge atom) charge-limit))

;; General feature utilities

(defn get-point [feature]
  (if (isa? (class feature) IAtom)
    (.getPoint3d feature)
    (ring-center feature)))

(defn extract-feature-points [features]
  (fmap (partial map get-point) features))

(defn find-features [molecule charge-limit]
  {
   "donor" (filter (partial is-donor? molecule) (.atoms molecule))
   "acceptor" (filter (partial is-acceptor? molecule) (.atoms molecule))
   "donor-and-acceptor" (filter (partial is-both-donor-and-acceptor? molecule) (.atoms molecule))
   "aromatic-rings"  (find-rings aromatic-tools  molecule)
   "aliphatic-rings" (find-rings aliphatic-tools molecule)
   "positive" (filter (partial is-positive?    charge-limit)  (.atoms molecule))
   "negative" (filter (partial is-negative? (- charge-limit)) (.atoms molecule))
   })

(defn steric-features
  [molecule]
  {"steric" (filter (comp not (is-atom? "H")) (.atoms molecule))})

;; gaussian-overlap calcualtions
(defn translate-rotate-translate-feature-points
  "Takes all the points from a map of feature points and
  applies the pre-translation, then the rotation and then
  the post-translation to all of the points.
  Returns a new map with the altered features.
  Translations are Point3ds, the rotation is a Matrix."
  [pre-translation rotation post-translation features]
  (->> features
       (fmap (partial map (partial vec-add pre-translation)))
       (fmap (partial map (partial rotate-point rotation)))
       (fmap (partial map (partial vec-add post-translation)))))

(defn apply-matrix-to-features
  "Applies the matrix to all the feature points.
  The features are assumes to be stored as Point3ds."
  [matrix features]
  (fmap (partial map (partial move-and-translate-point matrix)) features))

(defnk gaussian-overlap 
  "Calculates the Gaussian overlap between two sets of features
  stored in maps. Each value in the map is a coll of Point3d.
  Optional arguments are :scale."
  [features-1 features-2 :scale 1.0]
  (apply 
   +
   (flatten
    (for [feature-name (keys features-1)]
      (for [point-1 (features-1 feature-name)
	    point-2 (features-2 feature-name)]
	(Math/exp (/ (- (Math/pow (distance point-1 point-2) 2))
		     (Math/pow scale 2))))))))

; Functions for avoiding recalculation features for molecules that have been moved, rotated 
; and given a new configuration
(defn atom-id-from-atom-features [molecule features]
  (fmap 
   (partial 
    map 
    (fn [feature] (if (isa? (class feature) IAtom) 
		    (.getAtomNumber molecule feature)
		    (map #(.getAtomNumber molecule %) feature))))
   features))

(defn atom-from-atom-id-features [molecule features]
  (fmap
   (partial
    map
    (fn [id] (if (integer? id) 
	       (.getAtom molecule id)
	       (map #(.getAtom molecule %) id))))
   features))