(ns alijn.pharmacophore
  (:use [clj-todo.todo]
	[alijn.combinatorics])
  (:import 
   [org.openscience.cdk.smiles.smarts SMARTSQueryTool]
   [javax.vecmath Point3d]))

; Taken from 
; http://www.daylight.com/dayhtml_tutorials/languages/smarts/smarts_examples.html
; More advanced definitions available from same site.
(def example-pharmacophores
     {"hydrogen-bond acceptor" 
      "[!$([#6,F,Cl,Br,I,o,s,nX3,#7v5,#15v5,#16v4,#16v6,*+1,*+2,*+3])]",
      "hydrogen-bond donor" "[!$([#6,H0,-,-2,-3])]",
      "aromatic-5-ring" "C1CCCC1"})

(todo 
 "This function generates a new query-tool every time.
Can they be cached in a nice manner?"

(defn find-pharmacophore 
  "Result is a coll of atom collections, even though some pharmacophores
only match a single atom."
  [smarts-string molecule]
  (let [query-tool (SMARTSQueryTool. smarts-string)
	status (.matches query-tool molecule)]
    (if status
      (map (fn [indices] 
	     (map #(.getAtom molecule %) indices))
	   (.getUniqueMatchingAtoms query-tool))
      [])))
)

(defn get-center [atom & more]
  "Returns the center of the atoms as a Point3d."
  (let [atoms (cons atom more)
	points (map #(.getPoint3d %) atoms)
	result (Point3d. 0 0 0)]
    (doall (map #(.add result %) points))
    (.scale result (/ 1 (count atoms)))
    result))

(defn pharmacophore-groups
  "Extract the phamacophores defined in a {name smarts-string} map
from the molecule. Returns collection of {:name :centers}.
The centers are Point3d objects."
  [pharmacophores molecule]
  (map (fn [[name smarts-string]]
	 (let [groups (find-pharmacophore smarts-string molecule)
	       centers (map get-center groups)]
	   {:name name
	    :centers centers})
	 pharmacophores)))

; Pairing of pharmacopohores
(defn center-to-pharmacophore-map
  "Constructs a map from Point3d centers to pharmacophore types.
Input is a seq of {:name :centers}."
  [pharmacophores]
  (reduce 
   (fn [m {name :name centers :centers}]
     (reduce (fn [m center] (assoc m center name)) m centers)) 
   {} pharmacophores))

(todo
 "Problem with using maps for retaining pharmacophore type is that Point3d are
bad keys in the sense that two different objects that reference the same position
maps to the same. A problem if several pharmacophores are colocated.
The following pice of code demonstrates the problem."
 (comment
   (def origo (Point3d. 0 0 0))
   (def fake-origo (Point3d. 0 0 0))
   (def t (assoc (hash-map) origo :origo))
   (println (t origo))
   (println (t fake-origo))))

(defn pharmacophore-pairings
  "Returns all point pairings from two seqs of {:name :centers}.
Assumes the names arrives in same order in both seqeunces.
Result is (([reference-point subject-point] [t-point s-point] ...) ...)."
  [reference subject]
  (let [ref-center-map (center-to-pharmacophore-map reference)]
    (map 
     (partial 
      map
      (fn [[ref-point sub-point]] [(ref-center-map ref-point) [ref-point sub-point]]))
     (grouped-all-pairs 
      (map :centers reference) 
      (map :centers subject)))))