(ns alijn.molecule-alignment
  (:use clj-todo.todo)
  (:use [alijn.pharmacophore]
	[alijn.kabsch])
  (:import [javax.vecmath Point3d]))

(defn vec-sub 
  "u - v, where u and v are Point3d vectors."
  [u v]
  (let [result (Point3d.)]
    (.sub result u v)
    result))

(defn vec-center 
  "Finds the vector center for a seq of Point3d."
  [points]
  (let [result (Point3d. 0 0 0)]
    (doseq [point points] (.add result point))
    (.scale result (/ 1 (count points)))
    result))
  
(defn center-points
  [points]
  (let [center (vec-center points)
	moved-points (map #(vec-sub %1 center) points)] 
    moved-points))

(defn kabsch-on-pairing 
  [pairing]
  (let [pharm-names (map first pairing)
	points (map second pairing)
	ref-points (center-points (map first points))
	subj-points (center-points (map second points))
	subj-rotated (kabsch ref-points subj-points)
	deviation (rmsd ref-points subj-rotated)
	result (map vector pharm-names subj-rotated)]
    {:result result
     :rmsd deviation}))

(defn optimal-pharmacophore-alignment
  "Assumes pharmacophores arrive in same order, and that they are the same!"
  [reference-pharmacophores subject-pharmacophores]
  (let [pairings (pharmacophore-pairings 
		  reference-pharmacophores 
		  subject-pharmacophores)]
    (first
     (apply 
      sorted-set-by 
      (fn [{rmsd-1 :rmsd} {rmsd-2 :rmsd}] (compare rmsd-1 rmsd-2))
      (map kabsch-on-pairing pairings)))))
