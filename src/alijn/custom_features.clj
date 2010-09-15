(ns alijn.custom-features
  (:import [org.openscience.cdk Atom Bond AtomContainer]
	   [org.openscience.cdk.interfaces IAtom]
	   [org.openscience.cdk.smiles.smarts SMARTSQueryTool])
  (:use clojure.pprint
	[alijn math]))

;; Hydrogen donor and acceptor

(comment 
"Macro solution"
(defmacro create-is-atom? [type]
  (let [name (symbol (format "is-atom-%s?" type))
	upper-case-symbol (.toUpperCase type)]
  `(defn ~name [atm#] (= ~upper-case-symbol (.getSymbol atm#)))))

(create-is-atom? "h")
(create-is-atom? "c")
(create-is-atom? "o")
(create-is-atom? "n")
(create-is-atom? "s")
)

; (is-atom? "h") ; returns a fn that can be applied to atoms
; (is-atom? atm "h") ; generates function and applies
; (is-atom? atm "o" "n") ; generates functions and returns if any symbol is matched
(defn is-atom? 
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

;; General feature utilities

(defn get-point [feature]
  (if (isa? (class feature) IAtom)
    (.getPoint3d feature)
    (ring-center feature)))

(defn find-features [molecule]
  {"donor" (filter (partial is-donor? molecule) (.atoms molecule))
   "acceptor" (filter (partial is-acceptor? molecule) (.atoms molecule))
   "aromatic-rings" (find-aromatic-rings molecule)})

;; Charges

(comment

(defn charged-atoms [molecule charge-limit]
  [(filter #(>= (.getCharge %)    charge-limit)  (.atoms molecule))
   (filter #(<= (.getCharge %) (- charge-limit)) (.atoms molecule))])  

(let [mol (first (read-molecules "data/example/comt_ligands.mol2"))
      [pos neg] (charged-atoms mol 1)]

;  (prn "atoms:" (filter #(= "C" (.getSymbol %)) (.atoms mol)))
;  (prn "charges:" (map #(.getFormalCharge %) (.atoms mol)))
;  (prn "Positivly charged atoms:" (count pos))
;  (prn "Negativly charged atoms:" (count neg))

  (prn (first (.atoms mol)))
)


;(ChemFileManipulator/getAllAtomContainers (.read mol2-reader (new ChemFile)))

)




