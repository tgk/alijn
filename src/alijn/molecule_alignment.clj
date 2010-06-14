(ns alijn.molecule-alignment
  (:use clj-todo.todo)
  (:use [alijn pharmacophore kabsch combinatorics])
  (:import [javax.vecmath Point3d])
  (:import 
   [java.io File FileInputStream]
   [org.openscience.cdk DefaultChemObjectBuilder]
   [org.openscience.cdk.io.iterator IteratingMDLReader]))

; Bottom up code

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

; Top down code

(defn read-sdf-file [filename]
  (let [file (File. filename)
        stream (FileInputStream. file)
        reader 
          (IteratingMDLReader. stream (DefaultChemObjectBuilder/getInstance))]
    (iterator-seq reader)))
	
(defn group-by
  "Groups the seq by the keys generated from group-fn."
  [group-fn seq]
  (apply
   merge-with
   concat
   (map (fn [elm] {(group-fn elm) [elm]}) seq)))

(defn group-conformations-by-name
  [conformations]
  (group-by (fn [molecule] (.getName molecule)) conformations))

(defn map-on-values
  "Applies f to the values in the map m."
  [f m]
  (apply merge (map (fn [[k v]] {k (f v)}) m)))

(defn generate-pharmacophores
  [grouped-conformations]
  (map-on-values
   (fn [conformations] (map find-pharmacophore conformations))
   grouped-conformations))

(todo
 "I have the feeling this belongs further up, in the bottom up code.
Maybe even in alijn.pharmacophore."

(defn alignments-with-different-reference-pharmacophores
  "Iteratively keeps one set of pharmacophores as a reference while
aligningen the remaining N - 1 sets of pharmacophores to it.
Result is a seq of maps with :rmsd and :result, :result being a list of 
list of optimally aligned points."
  [pharmacophores]
  (map
   (fn [[reference subjects]]
     (let [result (map (partial optimal-pharmacophore-alignment reference) subjects)
	   rmsd-sum (reduce + (map :rmsd result))]
       {:rmsd-sum rmsd-sum
	:result result
	}))
   (leave-one-out pharmacophores)
  ))
)
  