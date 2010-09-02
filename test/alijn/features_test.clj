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

(deftest test-parse-smarts-block
  (is 
   (=
    [{:smarts "[NX3][#6](=[NX2,NX3+])[#6]" :points [2]}
     {:smarts "[NX2,NX3+]=[#6]([NH;X3])([NH;X3])" :points [0 2 3]}
     {:smarts "[NX2,NX3+]=[#6]([NX3])([NX3])" :points [0]}
     {:smarts "n1c([NH2])ccnc1([NH2])" :points [0]}
     {:smarts "[NX2,NX3+]=C([NX3])c1ccccc1" :points [0 1 2 3]}]
    (parse-smarts-block
     "[NX3][#6](=[NX2,NX3+])[#6]              group(3)   0   1   0   0   1   1.8
[NX2,NX3+]=[#6]([NH;X3])([NH;X3])       group(1,3,4)   0   1   0   0   1   1.8
[NX2,NX3+]=[#6]([NX3])([NX3])           group(1)   0   1   0   0   1   1.8
n1c([NH2])ccnc1([NH2])                  group(1)   0   1   0   0   1   1.8
[NX2,NX3+]=C([NX3])c1ccccc1             group(1,2,3,4)   0   1   0   0   1   1.8"))))

(deftest test-block-starting-with
  (let [s "#FEATURE
#IDENTIFIER P
#COMMENT Positive (P)
#INCLUDE 
[NX3][#6](=[NX2,NX3+])[#6]              group(3)   0   1   0   0   1   1.8
[NX2,NX3+]=[#6]([NH;X3])([NH;X3])       group(1,3,4)   0   1   0   0   1   1.8
[+]                                     group(1)   0   1   0   0   1   1.8
#EXCLUDE
[c;X2]1ccnc([NH2])n1                    group   0   1   0   0   1   1.8
[NH2][*]=,@[#7]                         group(1)   0   1   0   0   1   1.8
[+][-]                                  group   0   1   0   0   1   1.8"]
  (is (= "P" (block-starting-with "IDENTIFIER" s)))
  (is (= "Positive (P)" (block-starting-with "COMMENT" s)))
  (is (= 
"[NX3][#6](=[NX2,NX3+])[#6]              group(3)   0   1   0   0   1   1.8
[NX2,NX3+]=[#6]([NH;X3])([NH;X3])       group(1,3,4)   0   1   0   0   1   1.8
[+]                                     group(1)   0   1   0   0   1   1.8" 
(block-starting-with "INCLUDE" s)))
  (is (= 
"[c;X2]1ccnc([NH2])n1                    group   0   1   0   0   1   1.8
[NH2][*]=,@[#7]                         group(1)   0   1   0   0   1   1.8
[+][-]                                  group   0   1   0   0   1   1.8"
(block-starting-with "EXCLUDE" s)))))

(deftest test-parse-feature-block
  (is 
   (=
    {:identifier "P"
     :comment "Positive (P)"
     :include [{:smarts "[NX3][#6](=[NX2,NX3+])[#6]" :points [2]}
	       {:smarts "[NX2,NX3+]=[#6]([NH;X3])([NH;X3])" :points [0 2 3]}
	       {:smarts "[+]" :points [0]}]
     :exclude [{:smarts "[c;X2]1ccnc([NH2])n1" :points :all}
	       {:smarts "[NH2][*]=,@[#7]" :points [0]}
	       {:smarts "[+][-]" :points :all}]}
    (parse-feature-block
     "#FEATURE
#IDENTIFIER P
#COMMENT Positive (P)
#INCLUDE 
[NX3][#6](=[NX2,NX3+])[#6]              group(3)   0   1   0   0   1   1.8
[NX2,NX3+]=[#6]([NH;X3])([NH;X3])       group(1,3,4)   0   1   0   0   1   1.8
[+]                                     group(1)   0   1   0   0   1   1.8
#EXCLUDE
[c;X2]1ccnc([NH2])n1                    group   0   1   0   0   1   1.8
[NH2][*]=,@[#7]                         group(1)   0   1   0   0   1   1.8
[+][-]                                  group   0   1   0   0   1   1.8
")))
(is
 (=
  {:identifier "R"
   :comment "Aromatic Rings (R)"
   :include []
   :exclude []}
  (parse-feature-block
   "#FEATURE
#IDENTIFIER R
#COMMENT Aromatic Rings (R)
#INCLUDE 
default_aromatic_vector                 group   0   1   -8   0   1   1.8
default_aromatic_surface                group   0   1   0   1   1   1.8
#EXCLUDE
"))))

(deftest test-split-into-phase-blocks
  (is
   (=
    ["#VERSION 31212
#SAS_WMIN 42.675
#SAS_PROBE 1.4
#SAS_RESOLUTION 0.2"
"#IDENTIFIER A
#COMMENT Acceptor (A)
#INCLUDE 
[N;X1]#[#6]                             vector(1)   0   1   -4   0   1   1.8
[N;X1]#CC                               vector(1)   0   1   -4   0   1   1.8
[N;X2](=C~[C,c])C                       vector(1)   0   1   -3   0   1   1.8
[N;X2](O)=N[a]                          vector(1)   0   1   -3   0   1   1.8
[N;X2](=N-O)[a]                         vector(1)   0   1   -3   0   1   1.8
[n;X2]1ccccc1                           vector(1)   0   1   -3   0   1   1.8
[n;X2]([a])([a])                        vector(1)   0   1   -3   0   1   1.8
[N;X2](=C~[C,c])(~[*])                  vector(1)   0   1   -3   0   1   1.8
[N;X3](C)(C)[N;X3]C                     vector(1)   0   1   -2   0   1   1.8
[N;X2](=C)(~[*])                        vector(1)   0   1   -3   0   1   1.8
[N;X2](~[C,c])=[N;X2]                   vector(1)   0   1   -3   0   1   1.8
[n;X2]1c[nH]cc1                         vector(1)   0   1   -3   0   1   1.8
O=[S;X4](=O)([!#8])([!#8])              vector(1)   0   1   -7   0   1   1.8
[O;X2]C                                 vector(1)   0   1   -5   0   1   1.8
[O;X2]N                                 vector(1)   0   1   -5   0   1   1.8
[O;X1]=[C,c]                            vector(1)   0   1   -6   0   1   1.8
o                                       vector(1)   0   1   -3   0   1   1.8
[O;X2](C)C                              vector(1)   0   1   -5   0   1   1.8
[O;X2]c1ncccc1                          vector(1)   0   1   -5   0   1   1.8
[O;X2]~[a]                              vector(1)   0   1   -5   0   1   1.8
O=PO([!#1])                             vector(1)   0   1   -7   0   1   1.8
[O;X2]                                  vector(1)   0   1   -5   0   1   1.8
[S;X2](C)C                              vector(1)   0   1   -5   0   1   1.8
[S;X2](=C)N                             vector(1)   0   1   -5   0   1   1.8
#EXCLUDE
O=C[O-,OH]                              point   0   1   0   0   1   1.8
[O-,OH]C(=O)                            point   0   1   0   0   1   1.8
[nH]([a])[a]                            point   0   1   0   0   1   1.8
[#7;X3][*]=[O,S]                        point   0   1   0   0   1   1.8
[N;X3](C)(C)[C;X3]                      point   0   1   0   0   1   1.8
[N;X3][a]                               point   0   1   0   0   1   1.8
N(=N=N)[#6]                             point   0   1   0   0   1   1.8
[NH2](C(=O)[NH2])                       point   0   1   0   0   1   1.8
[NH](C=O)(C=O)                          point   0   1   0   0   1   1.8
[NH2](S(=O)(=O)[#6])[#6]                point   0   1   0   0   1   1.8
[NH](S(=O)(=O)[#6])[#6]                 point   0   1   0   0   1   1.8
n1c([NH2])ccnc1([NH2])                  point   0   1   0   0   1   1.8
o1nccc1                                 point   0   1   0   1   1   1.8
o1cncc1                                 point   0   1   0   1   1   1.8
o1cccc1                                 point   0   1   0   1   1   1.8
[O;X2]C=O                               point   0   1   0   1   1   1.8
[O;X2]                                  point   0   1   0   1   1   1.8"
"#IDENTIFIER D
#COMMENT Donor (D)
#INCLUDE 
[#1][O;X2]                              vector(1)   0   1   -1   0   1   1.8
[#1]S[#6]                               vector(1)   0   1   -1   0   1   1.8
[#1][C;X2]#[C;X2]                       vector(1)   0   1   -1   0   1   1.8
[#1][NX3]C(=[NX2])[#6]                  vector(1)   0   1   -1   0   1   1.8
[#1][#7]                                vector(1)   0   1   -1   0   1   1.8
#EXCLUDE
[#1]OC(=O)                              point(1)   0   1   0   0   1   1.8
[#1]O[S;X3]=O                           point(1)   0   1   0   0   1   1.8
[#1]O[S;X4](=O)(=O)                     point(1)   0   1   0   0   1   1.8
[#1]O[P;X3]=O                           point(1)   0   1   0   0   1   1.8
[#1]O[P;X4]=O                           point(1)   0   1   0   0   1   1.8
[#1]n1nnnc1                             point(1)   0   1   0   0   1   1.8
[#1]N([S;X4](=O)(=O))(C(F)(F)(F))       point(1)   0   1   0   0   1   1.8
[#1]([NH2;X3,NH3]([#6;X4]))             point(1)   0   1   0   0   1   1.8
[#1]([NH;X3,NH2]([#6;X4])([#6;X4]))     point(1)   0   1   0   0   1   1.8
[#1]([NH;X4]([#6;X4])([#6;X4])([#6;X4]))   point(1)   0   1   0   0   1   1.8
[#1][NX3]C(=[NX2])[NX3]                 point(1)   0   1   0   0   1   1.8
[#1][NX3]C(=[NX3+])                     point(1)   0   1   0   0   1   1.8
[#1][NX3+]=C[NH2]                       point(1)   0   1   0   0   1   1.8
[#1][NX3]C(=[NX2])                      point(1)   0   1   0   0   1   1.8
[#1][NX3][#6](=[NX2,NX3+])[#6]          point(1)   0   1   0   0   1   1.8"]
    (split-into-phase-blocks
     "#VERSION 31212
#SAS_WMIN 42.675
#SAS_PROBE 1.4
#SAS_RESOLUTION 0.2
#FEATURE
#IDENTIFIER A
#COMMENT Acceptor (A)
#INCLUDE 
[N;X1]#[#6]                             vector(1)   0   1   -4   0   1   1.8
[N;X1]#CC                               vector(1)   0   1   -4   0   1   1.8
[N;X2](=C~[C,c])C                       vector(1)   0   1   -3   0   1   1.8
[N;X2](O)=N[a]                          vector(1)   0   1   -3   0   1   1.8
[N;X2](=N-O)[a]                         vector(1)   0   1   -3   0   1   1.8
[n;X2]1ccccc1                           vector(1)   0   1   -3   0   1   1.8
[n;X2]([a])([a])                        vector(1)   0   1   -3   0   1   1.8
[N;X2](=C~[C,c])(~[*])                  vector(1)   0   1   -3   0   1   1.8
[N;X3](C)(C)[N;X3]C                     vector(1)   0   1   -2   0   1   1.8
[N;X2](=C)(~[*])                        vector(1)   0   1   -3   0   1   1.8
[N;X2](~[C,c])=[N;X2]                   vector(1)   0   1   -3   0   1   1.8
[n;X2]1c[nH]cc1                         vector(1)   0   1   -3   0   1   1.8
O=[S;X4](=O)([!#8])([!#8])              vector(1)   0   1   -7   0   1   1.8
[O;X2]C                                 vector(1)   0   1   -5   0   1   1.8
[O;X2]N                                 vector(1)   0   1   -5   0   1   1.8
[O;X1]=[C,c]                            vector(1)   0   1   -6   0   1   1.8
o                                       vector(1)   0   1   -3   0   1   1.8
[O;X2](C)C                              vector(1)   0   1   -5   0   1   1.8
[O;X2]c1ncccc1                          vector(1)   0   1   -5   0   1   1.8
[O;X2]~[a]                              vector(1)   0   1   -5   0   1   1.8
O=PO([!#1])                             vector(1)   0   1   -7   0   1   1.8
[O;X2]                                  vector(1)   0   1   -5   0   1   1.8
[S;X2](C)C                              vector(1)   0   1   -5   0   1   1.8
[S;X2](=C)N                             vector(1)   0   1   -5   0   1   1.8
#EXCLUDE
O=C[O-,OH]                              point   0   1   0   0   1   1.8
[O-,OH]C(=O)                            point   0   1   0   0   1   1.8
[nH]([a])[a]                            point   0   1   0   0   1   1.8
[#7;X3][*]=[O,S]                        point   0   1   0   0   1   1.8
[N;X3](C)(C)[C;X3]                      point   0   1   0   0   1   1.8
[N;X3][a]                               point   0   1   0   0   1   1.8
N(=N=N)[#6]                             point   0   1   0   0   1   1.8
[NH2](C(=O)[NH2])                       point   0   1   0   0   1   1.8
[NH](C=O)(C=O)                          point   0   1   0   0   1   1.8
[NH2](S(=O)(=O)[#6])[#6]                point   0   1   0   0   1   1.8
[NH](S(=O)(=O)[#6])[#6]                 point   0   1   0   0   1   1.8
n1c([NH2])ccnc1([NH2])                  point   0   1   0   0   1   1.8
o1nccc1                                 point   0   1   0   1   1   1.8
o1cncc1                                 point   0   1   0   1   1   1.8
o1cccc1                                 point   0   1   0   1   1   1.8
[O;X2]C=O                               point   0   1   0   1   1   1.8
[O;X2]                                  point   0   1   0   1   1   1.8
#FEATURE
#IDENTIFIER D
#COMMENT Donor (D)
#INCLUDE 
[#1][O;X2]                              vector(1)   0   1   -1   0   1   1.8
[#1]S[#6]                               vector(1)   0   1   -1   0   1   1.8
[#1][C;X2]#[C;X2]                       vector(1)   0   1   -1   0   1   1.8
[#1][NX3]C(=[NX2])[#6]                  vector(1)   0   1   -1   0   1   1.8
[#1][#7]                                vector(1)   0   1   -1   0   1   1.8
#EXCLUDE
[#1]OC(=O)                              point(1)   0   1   0   0   1   1.8
[#1]O[S;X3]=O                           point(1)   0   1   0   0   1   1.8
[#1]O[S;X4](=O)(=O)                     point(1)   0   1   0   0   1   1.8
[#1]O[P;X3]=O                           point(1)   0   1   0   0   1   1.8
[#1]O[P;X4]=O                           point(1)   0   1   0   0   1   1.8
[#1]n1nnnc1                             point(1)   0   1   0   0   1   1.8
[#1]N([S;X4](=O)(=O))(C(F)(F)(F))       point(1)   0   1   0   0   1   1.8
[#1]([NH2;X3,NH3]([#6;X4]))             point(1)   0   1   0   0   1   1.8
[#1]([NH;X3,NH2]([#6;X4])([#6;X4]))     point(1)   0   1   0   0   1   1.8
[#1]([NH;X4]([#6;X4])([#6;X4])([#6;X4]))   point(1)   0   1   0   0   1   1.8
[#1][NX3]C(=[NX2])[NX3]                 point(1)   0   1   0   0   1   1.8
[#1][NX3]C(=[NX3+])                     point(1)   0   1   0   0   1   1.8
[#1][NX3+]=C[NH2]                       point(1)   0   1   0   0   1   1.8
[#1][NX3]C(=[NX2])                      point(1)   0   1   0   0   1   1.8
[#1][NX3][#6](=[NX2,NX3+])[#6]          point(1)   0   1   0   0   1   1.8"))))