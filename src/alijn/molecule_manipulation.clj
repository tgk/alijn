(ns alijn.molecule-manipulation
  (:use alijn.math)
  (:import javax.vecmath.Point3d))

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

; Might be deprectaed
(defn translate-and-rotate-molecule
  "Generates a new molecule that has been first translated using the
Point3d translation vector and the rotated using the Jama.Matrix."
  [translation rotation molecule]
  (let [clone (.clone molecule)
	atoms (.atoms clone)]
    (doseq [atom atoms] 
      (translate-atom! translation atom)
      (rotate-atom! rotation atom))
    clone))

(defn translate-rotate-and-translate-molecule
  "Generates a new molecule that has been first rotated using the 
  Jama.Matrix and then translated using the Point3d."
  [^javax.vecmath.Point3d pre-translation 
   ^Jama.Matrix rotation 
   ^javax.vecmath.Point3d post-translation 
   molecule]
  (let [clone (.clone molecule)
	atoms (.atoms clone)]
    (doseq [atom atoms] 
      (translate-atom! pre-translation atom)
      (rotate-atom! rotation atom)
      (translate-atom! post-translation atom))
    clone))

(defn center-of-mass 
  "Calculates the average position of all atoms in the molecule.
  Atoms are not weighted."
  [molecule]
  (vec-center (map #(.getPoint3d %) (.atoms molecule))))

;;; 4D
(defn apply-matrix-to-atom! [matrix a]
  (.setPoint3d a (move-and-translate-point matrix (.getPoint3d a))))

(defn apply-matrix-to-molecule [matrix molecule]
  (let [clone (.clone molecule)
	atoms (.atoms clone)]
    (doseq [a atoms] (apply-matrix-to-atom! matrix a))
    clone))

;;; Using 4D
(defn randomise-molecule-orientation [molecule]
  (let [rand-axis-coord (fn [] (- (rand (* 4 Math/PI)) (* 2 Math/PI)))
	random-rotation (Point3d. (rand-axis-coord) (rand-axis-coord) (rand-axis-coord))
	center (center-of-mass molecule)]
    (apply-matrix-to-molecule
     (matrix-product
      (translation-matrix center)
      (rotation-matrix random-rotation)
      (translation-matrix (neg center)))
     molecule)))

(defn move-molecule-center [molecule new-center]
  (apply-matrix-to-molecule
   (translation-matrix (vec-sub new-center (center-of-mass molecule)))
   molecule))