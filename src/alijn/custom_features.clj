(ns alijn.custom-features
  (:import [org.openscience.cdk Atom Bond AtomContainer]
	   [org.openscience.cdk.interfaces IAtom]
	   [org.openscience.cdk.smiles.smarts SMARTSQueryTool])
  (:use clojure.pprint
	[alijn math]))

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

(def ring-tools (map #(SMARTSQueryTool. %) ["a1aaaa1" "a1aaaaa1" "a1aaaaaa1"]))

(defn find-aromatic-rings [container]
  (apply 
   concat
   (for [tool ring-tools]
     (when (.matches tool container)
       (for [indices (.getMatchingAtoms tool)]
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

(defn find-features [molecule]
  {
   "donor" (filter (partial is-donor? molecule) (.atoms molecule))
   "acceptor" (filter (partial is-acceptor? molecule) (.atoms molecule))
   "aromatic-rings" (find-aromatic-rings molecule)
   "positive" (filter (partial is-positive?  0.5) (.atoms molecule))
   "negative" (filter (partial is-negative? -0.5) (.atoms molecule))
   })

