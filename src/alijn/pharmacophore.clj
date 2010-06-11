(ns alijn.pharmacophore
  (:use [clj-todo.todo]
	[clojure.contrib.combinatorics])
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

; Pairing of elements
(defn shorter-permutations 
  "Lazy seq of all permutations of elements in items with
   only n items in returned sequences."
  [items n]
  (let [n (min n (count items))]
    (apply 
     concat 
     (map permutations 
	  (combinations items n)))))

(todo 
 "Should this be pushed to clojure.contrib? 
Might start with submitting it to mailing list"

(defn all-pairs 
  "Lazy seq of all pairings of elements from the two sequences."
  [seq-1 seq-2]
  (let [n (count seq-1) m (count seq-2)]
    (if (> n m)
      (map (partial map vector) 
	   (shorter-permutations seq-1 m)
	   (repeat seq-2))
      (map (partial map vector) 
	   (repeat seq-1)
	   (shorter-permutations seq-2 n)))))
)

(defn grouped-all-pairs 
  "Generates all grouped pairs as in
[[:f1 :f2] [:b1]] [[:f3] [:b2 :b3]] => 
[[[:f1 :f3] [:b1 :b2]] [[:f1 :f3] [:b1 :b3]] [[:f2 :f3] [:b1 :b2]] [[:f2 :f3] [:b1 :b3]]]"
  [target-groups subject-groups]
  (->> (map all-pairs target-groups subject-groups)
       (apply cartesian-product)
       (map (partial apply concat))))

; Pairing of pharmacopohores
(defn pharmacophore-pairings
  "Returns all point pairings from two seqs of {:name :centers}.
Assumes the names arrives in same order in both seqeunces.
Result is (([target-point subject-point] [t-point s-point] ...) ...)."
  [target subject]
  (grouped-all-pairs (map :centers target) (map :centers subject)))