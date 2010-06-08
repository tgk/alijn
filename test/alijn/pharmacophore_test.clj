(ns alijn.pharmacophore-test
  (:use [alijn.pharmacophore] :reload-all)
  (:use [clojure.test])
  (:use [clj-todo.todo])
  (:import 
   [org.openscience.cdk Atom DefaultChemObjectBuilder]
   [org.openscience.cdk.smiles SmilesParser]
   [javax.vecmath Point3d]))

;;; All tests uses the example-pharmacopohores from alijn.pharmacophore

(def smiles-parser (new SmilesParser (DefaultChemObjectBuilder/getInstance)))
(defn molecule-from-smiles [smiles-string] 
  (.parseSmiles smiles-parser smiles-string))

;;; Test structures ;;;

; Has three hydrogen acceptors and one 5-ring
(def pyrethrin 
     (molecule-from-smiles 
      "COC(=O)C(\\C)=C\\C1C(C)(C)[C@H]1C(=O)O[C@@H]2C(C)=C(C(=O)C2)CC=CC=C"))

; Has one hydrogen donor
(def flavopereirin
     (molecule-from-smiles 
      "CCc(c1)ccc2[n+]1ccc3c2Nc4c3cccc4"))

; Has two OH that are both donors and acceptors
(def oenanthotoxin
     (molecule-from-smiles
      "CCC[C@@H](O)CC\\C=C\\C=C\\C#CC#C\\C=C\\CO"))

;;;;;;;;;; TESTS ;;;;;;;;;;;;;

;;; Test of pharmacophore identifier, find-pharmacophore
(def smarts-hydrogen-acceptor (example-pharmacophores "hydrogen-bond acceptor"))
(def smarts-hydrogen-donor    (example-pharmacophores "hydrogen-bond donor"))
(def smarts-aromatic-5-ring   (example-pharmacophores "aromatic-5-ring"))

(deftest test-find-pharmacophore-by-counting
  ; Hydrogen acceptor count
  (todo "I can only see three, but it might be five..."
  (is (= 5 (count (find-pharmacophore smarts-hydrogen-acceptor pyrethrin)))))
  (is (= 0 (count (find-pharmacophore smarts-hydrogen-acceptor flavopereirin))))
  (is (= 2 (count (find-pharmacophore smarts-hydrogen-acceptor oenanthotoxin))))
  ; Hydrogen donor count
  (is (= 0 (count (find-pharmacophore smarts-hydrogen-donor pyrethrin))))
  (is (= 1 (count (find-pharmacophore smarts-hydrogen-donor flavopereirin))))
  (is (= 2 (count (find-pharmacophore smarts-hydrogen-donor oenanthotoxin))))
  ; 5-ring of carbon count
  (is (= 1 (count (find-pharmacophore smarts-aromatic-5-ring pyrethrin))))
  (is (= 0 (count (find-pharmacophore smarts-aromatic-5-ring flavopereirin))))
  (is (= 0 (count (find-pharmacophore smarts-aromatic-5-ring oenanthotoxin)))))

;;; Test of get-center
(def epsilon 0.00001)

(defn atom-dist [a1 a2]
  (let [p1 (.getPoint3d a1)
	p2 (.getPoint3d a2)]
    (.distance p1 p2)))

(defn same-position? [a1 a2]
  (if (< (atom-dist a1 a2) epsilon) true false))

(deftest test-get-center
  ; Test same-position?
  (is 
   (same-position? (dummy-atom 0 0 0)
		   (dummy-atom 0 0 0)))
  (is 
   (same-position? (dummy-atom (- epsilon 0.000001) 0 0)
		   (dummy-atom 0 0 0)))
  (is (not
       (same-position? (dummy-atom (+ 0.000001 epsilon) 0 0)
		       (dummy-atom 0 0 0))))
  ; 1D
  (is
   (same-position? (dummy-atom 1 2 3) 
		   (get-center (dummy-atom 1 2 3))))
  (is
   (same-position? (dummy-atom 1 0 0) 
		   (get-center (dummy-atom 0 0 0) (dummy-atom 2 0 0))))
  (is (not
       (same-position? (dummy-atom 10 0 0) 
		       (get-center (dummy-atom 0 0 0) (dummy-atom 2 0 0)))))
  (is
   (same-position? (dummy-atom 1 0 0) 
		   (get-center (dummy-atom 0 0 0)
			       (dummy-atom 1 0 0)
			       (dummy-atom 2 0 0))))
  ; 2D
  (is
   (same-position? (dummy-atom 1 1 0) 
		   (get-center (dummy-atom 0 0 0) (dummy-atom 2 2 0))))
  ; 3D
  (is
   (same-position? (dummy-atom 1 1 1) 
		   (get-center (dummy-atom 3 0 0) 
			       (dummy-atom 0 3 0)
			       (dummy-atom 0 0 3))))
  (is
   (same-position? (dummy-atom 1 1 1) 
		   (get-center (dummy-atom 0 0 0) 
			       (dummy-atom 2 2 2))))
)

;;; Unimplemented tests
(comment deftest test-pharmacophore-groups-types
  (is false))

