(ns alijn.features-test
  (:use [alijn.features] :reload-all)
  (:use [clojure.test])
  (:import 
   [org.openscience.cdk Atom DefaultChemObjectBuilder]
   [org.openscience.cdk.smiles SmilesParser]
   [javax.vecmath Point3d]))

(defn dummy-atom 
  ([point] (Atom. "C" point))
  ([x y z] (dummy-atom (Point3d. x y z))))

(def example-features
     {"hydrogen-bond acceptor" 
      [["[!$([#6,F,Cl,Br,I,o,s,nX3,#7v5,#15v5,#16v4,#16v6,*+1,*+2,*+3])]"] []],
      "hydrogen-bond donor" 
      [["[!$([#6,H0,-,-2,-3])]"] []],
      "aromatic-5-ring" 
      [["C1CCCC1"] []]})

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

;;; Test of feature identifier, find-feature
(def smarts-hydrogen-acceptor (example-features "hydrogen-bond acceptor"))
(def smarts-hydrogen-donor    (example-features "hydrogen-bond donor"))
(def smarts-aromatic-5-ring   (example-features "aromatic-5-ring"))

;;; Test of get-center
(def epsilon 0.00001)

(defn same-position? [u v] (< (.distance u v) epsilon))

(deftest test-get-center
  ; Test same-position?
  (is (same-position? (Point3d. 0 0 0) (Point3d. 0 0 0)))
  (is (same-position? (Point3d. (- epsilon 0.000001) 0 0) (Point3d. 0 0 0)))
  (is (not (same-position? (Point3d. (+ 0.000001 epsilon) 0 0) (Point3d. 0 0 0))))
  ; 1D
  (is (same-position? (Point3d. 1 2 3) (get-center [(dummy-atom 1 2 3)])))
  (is (same-position? (Point3d. 1 0 0) (get-center [(dummy-atom 0 0 0) (dummy-atom 2 0 0)])))
  (is (not (same-position? (Point3d. 10 0 0) (get-center [(dummy-atom 0 0 0) (dummy-atom 2 0 0)]))))
  (is (same-position? (Point3d. 1 0 0) 
		      (get-center [(dummy-atom 0 0 0)
				   (dummy-atom 1 0 0)
				   (dummy-atom 2 0 0)])))
  ; 2D
  (is (same-position? (Point3d. 1 1 0) (get-center [(dummy-atom 0 0 0) (dummy-atom 2 2 0)])))
  ; 3D
  (is (same-position? (Point3d. 1 1 1) 
		      (get-center [(dummy-atom 3 0 0) 
				   (dummy-atom 0 3 0)
				   (dummy-atom 0 0 3)])))
  (is (same-position? (Point3d. 1 1 1) 
		      (get-center [(dummy-atom 0 0 0) 
				   (dummy-atom 2 2 2)])))
  (is (same-position? (Point3d. 1 1 1) 
		      (get-center [(dummy-atom 0 0 0) 
				   (dummy-atom 2 2 2)]))))

; Phase parsing

(deftest test-parse-smarts-line
  (is 
   (= {:smarts "[NX3][#6](=[NX2,NX3+])[#6]" 
       :points [2]}
      (parse-smarts-line 
       "[NX3][#6](=[NX2,NX3+])[#6]              group(3)   0   1   0   0   1   1.8")))
  (is
   (= 
    {:smarts "[NX2,NX3+]=[#6]([NH;X3])([NH;X3])"
     :points [0, 2, 3]}
    (parse-smarts-line
     "[NX2,NX3+]=[#6]([NH;X3])([NH;X3])       group(1,3,4)   0   1   0   0   1   1.8")))
  (is
   (=
    {:smarts "[#1][C;X2]#[C;X2]"
     :points [0]}
    (parse-smarts-line
     "[#1][C;X2]#[C;X2]                       vector(1)   0   1   -1   0   1   1.8")))
  (is
   (=
    {:smarts "[#1]([NH2;X3,NH3]([#6;X4]))"
     :points [0]}
    (parse-smarts-line
     "[#1]([NH2;X3,NH3]([#6;X4]))             point(1)   0   1   0   0   1   1.8")))
  (is
   (=
    {:smarts "[N;X2](=N-O)[a]"
     :points [0]}
    (parse-smarts-line
     "[N;X2](=N-O)[a]                         vector(1)   0   1   -3   0   1   1.8")))
  ; With no numbers
  (is
   (=
    {:smarts "[NH2](C(=O)[NH2])"
     :points [0]}
    (parse-smarts-line
     "[NH2](C(=O)[NH2])                       point   0   1   0   0   1   1.8")))
  (is
   (=
    {:smarts "Br"
     :points :all}
    (parse-smarts-line
     "Br                                      group   0   1   0   0   1   1.8")))
  (is
   (=
    {:smarts "n1nc[nH]n1"
     :points :all}
    (parse-smarts-line
     "n1nc[nH]n1                              group   0   1   0   0   1   1.8"))))



(deftest test-parse-phase-with-tags
  (is (=
       {"A"
	{:exclude 
	 [{:smarts "smarts-3", :points [0]}],
	 :include
	 [{:smarts "smarts-1", :points [0]}
	  {:smarts "smarts-2", :points :all}]},
	"D" 
	{:include 
	 [{:smarts "smarts-4", :points [0]}]}}
       (parse-phase-with-tags "data/example/test_smarts.def"))))
       