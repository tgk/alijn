(ns alijn.energy
  (:import org.openscience.cdk.charges.MMFF94PartialCharges))

(def charge-calculator (MMFF94PartialCharges.))

(defn total-MMFF94-charges!
  "Calculates the MMFF94 charge of every atom and
  returns the total charge of the molecule.
  Alters molecule by adding the property 'MMFF94charge'
  to every atom."
  [molecule]
  (.calculateCharges charge-calculator molecule)
  (reduce + (map #(.getProperty % "MMFF94charge") (.atoms molecule))))