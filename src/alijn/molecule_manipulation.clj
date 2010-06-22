(ns alijn.molecule-manipulation
  (:use
   [clj-todo todo]
   [alijn math]))

(defn translate-atom! 
  "Performs an inplace translation of the atom."
  [translation atom]
  (let [original-point (.getPoint3d atom)
	new-point (vec-add original-point translation)]
    (.setPoint3d atom new-point)
    atom))

(defn rotate-atom!
  "Performs an inplace rotation of the atom."
  [rotation atom]
  (let [original-point (.getPoint3d atom)
	new-point (rotate-point rotation original-point)]
    (.setPoint3d atom new-point)
    atom))

(defn translate-and-rotate-molecule
  "Generates a new molecule that has been first translated using the
Point3d translation vector and the rotated using the Jama.Matrix."
  [translation rotation molecule]
  (let [clone (.clone molecule)
	atoms (.getAtoms clone)]
    (doseq [atom atoms] 
      (translate-atom! translation atom)
      (rotate-atom! rotation atom))
    clone))