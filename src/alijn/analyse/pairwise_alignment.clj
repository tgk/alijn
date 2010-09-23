(ns alijn.analyse.pairwise-alignment
  (:use [alijn io features pairwise-alignment math molecule-utils utils]
	clojure.contrib.command-line
	clojure.contrib.generic.functor
	clojure.contrib.profile
	clojure.pprint))

(def desc 
"Reads the molecules from one or more files. For each file, performs
pairwise alignment between every molecule in these and prints a 
table with the results.
Molecule names are truncated such that 1kcb_h_min becomes 1kcb.
If a file contains multiple molecules with the same name, the first
molecule is the native binding mode and the remaining are 
conformations. If only one molecule exists for each name they are
all native conformations that are sought re-aligned.")

(defn short-name    [molecule] (first (.split (molecule-name molecule) "_")))  

(defn get-best-rmsd 
  [threshold charge-limit constant-molecule [native-variable & variable-molecules]]
  (molecule-rmsd
   native-variable
   (align-with-multiple-variable 
     threshold constant-molecule variable-molecules charge-limit)))

(defn count-successes
  [threshold charge-limit success-rmsd grouped-ligands]
  (fmap
   (fn [conformations]
     (let [reference (first conformations)
	   best-rmsds (map (partial get-best-rmsd threshold charge-limit reference)
			   (vals grouped-ligands))]
       (count (filter #(<= % success-rmsd) best-rmsds))))
   grouped-ligands))

(defn 
  #^{:doc desc}
  align-and-show-table
  [& args]
  (profile
  (with-command-line args desc
    [[threshold "The threshold used by the clique detection algorithm." nil]
     [charge-limit "Limit for classifing atom as charged." "0.5"]
     [success-rmsd "The maximum rmsd for a realignment to be a success." "2.5"]
     filenames]
    (let [threshold (when threshold (Double/parseDouble threshold))
	  charge-limit (Double/parseDouble charge-limit)
	  success-rmsd (Double/parseDouble success-rmsd)]
      (print-table
       (cons
	["target" "avg. success" "min" "max" "ligands"]	
	(for [filename filenames]
	  (let [molecules (read-molecules filename)
		target-name (first (.split (last (.split filename "/")) "\\."))
		grouped-ligands (group-by short-name molecules)
		grouped-ligands (if (apply = 1 (vals (fmap count grouped-ligands))) 
				  (group-by short-name (concat molecules molecules)) 
				  grouped-ligands)
		successes (count-successes threshold charge-limit success-rmsd 
					   grouped-ligands)
		success-rates (fmap #(int (* 100 (/ % (count grouped-ligands)))) 
				    successes)]
	    [target-name 
	     (str (int (average (vals success-rates))))
	     (str (apply min (vals success-rates)))
	     (str (apply max (vals success-rates)))
	     (str (count grouped-ligands))]))))))))
       
(def test-file-1 "data/example/carboxypth-a.mol2")
(def test-file-2 "data/example/concanavalin.mol2")
(def test-file-3 "data/grouped/flexs/g-phosphorylase.mol2")

(defn test-and-show []
  (align-and-show-table
   "--threshold" "1" 
   test-file-1))