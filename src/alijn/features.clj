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
  (or (is-both-donor-and-acceptor? container atm)
      (and (is-atom? atm "S" "N") (connected-to-hydrogen? container atm))))

(defn is-acceptor? [container atm]
  (or (is-both-donor-and-acceptor? container atm)
      (and (is-atom? atm "O" "N") (not (connected-to-hydrogen? container atm)))))

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
   "aromatic-rings"  (find-rings aromatic-tools  molecule)
   "aliphatic-rings" (find-rings aliphatic-tools molecule)
   "positive" (filter (partial is-positive?    charge-limit)  (.atoms molecule))
   "negative" (filter (partial is-negative? (- charge-limit)) (.atoms molecule))
   })

;; gaussian-overlap calcualtions
(defnk gaussian-overlap 
  "Calculates the gaussian overlap between two sets of features
  stored in maps. Each value in the map is a coll of Point3d.
  Optional arguments are :scale."
  [features-1 features-2 :scale 0.5]
  (comment Math/exp (/ (- (Math/pow dist 2)) (Math/pow scale 2))))