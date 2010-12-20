(ns alijn.molecule-manipulation
  (:use alijn.math)
  (:import javax.vecmath.Point3d))

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

(defn random-translation [molecule]
  (let [random-center (Point3d. (- 10 (* 20 (rand)))
				(- 10 (* 20 (rand)))
				(- 10 (* 20 (rand))))]
    (move-molecule-center molecule random-center)))