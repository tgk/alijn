(ns alijn.pharmacophore
  (:use [clj-todo.todo])
  (:import 
   [org.openscience.cdk.smiles.smarts SMARTSQueryTool]
   [org.openscience.cdk Atom DefaultChemObjectBuilder]
   [org.openscience.cdk.smiles SmilesParser]
   [javax.vecmath Point3d]))

(defn dummy-atom 
  ([point] (Atom. "C" point))
  ([x y z] (dummy-atom (Point3d. x y z))))

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
       []))))

(defn get-center [atom & more]
  "Returns the center of the atoms as a new atom."
  (let [atoms (cons atom more)
	points (map #(.getPoint3d %) atoms)
	result (Point3d. 0 0 0)]
    (doall (map #(.add result %) points))
    (.scale result (/ 1 (count atoms)))
    (dummy-atom result)))

(defn pharmacopohore-groups 
  "Extract the phamacophores defined in a {name smarts-string} map
from the molecule. Returns collection of {:name :atoms :center} structs."
  [pharmacophores molecule]
  nil)

