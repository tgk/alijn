(ns alijn.pharmacophore-test
  (:use [alijn.pharmacophore] :reload-all)
  (:use [clojure.test])
  (:use [clj-todo.todo])
  (:import 
   [org.openscience.cdk Atom DefaultChemObjectBuilder]
   [org.openscience.cdk.smiles SmilesParser]
   [javax.vecmath Point3d]))

(defn dummy-atom 
  ([point] (Atom. "C" point))
  ([x y z] (dummy-atom (Point3d. x y z))))

;;; All tests uses the example-pharmacopohores from alijn.pharmacophore

;;; Test structures ;;;
(def smiles-parser (new SmilesParser (DefaultChemObjectBuilder/getInstance)))
(defn molecule-from-smiles [smiles-string] 
  (.parseSmiles smiles-parser smiles-string))

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

(defn same-position? [u v] (< (.distance u v) epsilon))

(deftest test-get-center
  ; Test same-position?
  (is (same-position? (Point3d. 0 0 0) (Point3d. 0 0 0)))
  (is (same-position? (Point3d. (- epsilon 0.000001) 0 0) (Point3d. 0 0 0)))
  (is (not (same-position? (Point3d. (+ 0.000001 epsilon) 0 0) (Point3d. 0 0 0))))
  ; 1D
  (is (same-position? (Point3d. 1 2 3) (get-center (dummy-atom 1 2 3))))
  (is (same-position? (Point3d. 1 0 0) (get-center (dummy-atom 0 0 0) (dummy-atom 2 0 0))))
  (is (not (same-position? (Point3d. 10 0 0) (get-center (dummy-atom 0 0 0) (dummy-atom 2 0 0)))))
  (is (same-position? (Point3d. 1 0 0) 
		      (get-center (dummy-atom 0 0 0)
				  (dummy-atom 1 0 0)
				  (dummy-atom 2 0 0))))
  ; 2D
  (is (same-position? (Point3d. 1 1 0) (get-center (dummy-atom 0 0 0) (dummy-atom 2 2 0))))
  ; 3D
  (is (same-position? (Point3d. 1 1 1) 
		      (get-center (dummy-atom 3 0 0) 
				  (dummy-atom 0 3 0)
				  (dummy-atom 0 0 3))))
  (is (same-position? (Point3d. 1 1 1) 
		      (get-center (dummy-atom 0 0 0) 
				  (dummy-atom 2 2 2))))
  (is (same-position? (Point3d. 1 1 1) 
		      (apply get-center [(dummy-atom 0 0 0) 
					 (dummy-atom 2 2 2)]))))

;;; Unimplemented tests
(comment deftest test-pharmacophore-groups-types
  (is false))

;;; Ugly ugly println statements
(todo
 "Can't use SMILES strings for this, as they do not have 3D information.
Running this results in a null pointer exception."

(comment
  (println (pharmacophore-groups example-pharmacophores pyrethrin))
  (println (pharmacophore-groups example-pharmacophores flavopereirin))
  (println (pharmacophore-groups example-pharmacophores oenanthotoxin)))
)

(deftest test-center-to-pharmacophore-map
  (is (= {:c1 "foo", :c2 "foo", :c3 "bar"}
	 (center-to-pharmacophore-map [{:name "foo" :centers [:c1 :c2]}
					{:name "bar" :centers [:c3]}
					{:name "baz" :centers []}]))))

(deftest test-pharmacophore-pairings
  (is (= '(())
	 (pharmacophore-pairings [{:name "foo" :centers nil} 
				  {:name "bar" :centers nil}]
				 [{:name "foo" :centers nil} 
				  {:name "bar" :centers nil}])))
  (is (= '((["foo" [:f1 :f3]] ["foo" [:f2 :f4]] ["bar" [:b1 :b2]])
	   (["foo" [:f1 :f3]] ["foo" [:f2 :f4]] ["bar" [:b1 :b3]])
	   (["foo" [:f1 :f4]] ["foo" [:f2 :f3]] ["bar" [:b1 :b2]])
	   (["foo" [:f1 :f4]] ["foo" [:f2 :f3]] ["bar" [:b1 :b3]]))
	 (pharmacophore-pairings [{:name "foo" :centers [:f1 :f2]} 
				  {:name "bar" :centers [:b1]}]
				 [{:name "foo" :centers [:f3 :f4]} 
				  {:name "bar" :centers [:b2 :b3]}])))
  (is (= '((["foo" [:f1 :f3]] ["foo" [:f2 :f4]] ["bar" [:b1 :b2]] ["baz" [:c1 :c2]])
	   (["foo" [:f1 :f3]] ["foo" [:f2 :f4]] ["bar" [:b1 :b3]] ["baz" [:c1 :c2]])
	   (["foo" [:f1 :f4]] ["foo" [:f2 :f3]] ["bar" [:b1 :b2]] ["baz" [:c1 :c2]])
	   (["foo" [:f1 :f4]] ["foo" [:f2 :f3]] ["bar" [:b1 :b3]] ["baz" [:c1 :c2]]))
	 (pharmacophore-pairings [{:name "foo" :centers [:f1 :f2]} 
				  {:name "bar" :centers [:b1]}
				  {:name "baz" :centers [:c1]}]
				 [{:name "foo" :centers [:f3 :f4]} 
				  {:name "bar" :centers [:b2 :b3]}
				  {:name "baz" :centers [:c2]}]))))

