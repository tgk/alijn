(ns alijn.analyse.numerical-optimiser-pairwise-alignment
  (:use [alijn 
	 io 
	 features 
	 differential-evolution 
	 numerical-optimiser-pairwise-alignment 
	 math 
	 molecule-utils 
	 utils]
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

(defn short-name [molecule] (first (.split (molecule-name molecule) "_")))  

(defn get-best-rmsd 
  [flexible-dihedral?
   charge-limit 
   feature-scale steric-scale 
   optimiser constant-molecule [native-variable & variable-molecules]]
  (molecule-rmsd
   native-variable
   (align-with-multiple-variable 
     flexible-dihedral? 
     charge-limit 
     feature-scale steric-scale 
     optimiser 
     constant-molecule variable-molecules)))

(defn count-successes
  [flexible-dihedral?
   charge-limit 
   feature-scale steric-scale
   optimiser success-rmsd grouped-ligands]
  (fmap
   (fn [conformations]
     (let [reference (first conformations)
	   best-rmsds (map (partial get-best-rmsd
				    flexible-dihedral?
				    charge-limit 
				    feature-scale steric-scale
				    optimiser reference)
			   (vals grouped-ligands))]
       (count (filter #(<= % success-rmsd) best-rmsds))))
   grouped-ligands))

(defn 
  #^{:doc desc}
  align-and-show-table
  [& args]
  (with-command-line args desc
    [[rigid-molecule? "Should dihedral angles be flexible or not." nil]
     [charge-limit "Limit for classifing atom as charged." "0.5"]
     [feature-scale "Scale to be used for Gaussian overlap" "1.0"]
     [steric-scale  "Scale to be used for Gaussian overlap" "0.5"]
     [success-rmsd "The maximum rmsd for a realignment to be a success." "2.5"]
     [n "Number of individuals in DE" "50"]
     [scaling-factor "Scaling factor in DE" "0.5"]
     [crossover-rate "Crossover rate in DE" "0.75"]
     [iterations "Iterations in DE" "100"]
     filenames]
    (let [flexible-dihedral? (not rigid-molecule?)
	  charge-limit (Double/parseDouble charge-limit)
	  feature-scale (Double/parseDouble feature-scale)
	  steric-scale  (Double/parseDouble steric-scale)
	  success-rmsd (Double/parseDouble success-rmsd)
	  n (Integer/parseInt n)
	  scaling-factor (Double/parseDouble scaling-factor)
	  crossover-rate (Double/parseDouble crossover-rate)
	  iterations (Integer/parseInt iterations)
	  optimiser (de-optimiser n scaling-factor crossover-rate iterations)]
      (println "Using flexible dihedral:" flexible-dihedral?)
      (print-table
       (cons
	["target" "avg. success" "min" "max" "ligands" "avg. features" "min" "max"]	
	(for [filename filenames]
	  (let [molecules (read-molecules filename)
		target-name (first (.split (last (.split filename "/")) "\\."))
		grouped-ligands (group-by short-name molecules)
		grouped-ligands (if (apply = 1 (vals (fmap count grouped-ligands))) 
				  (group-by short-name (concat molecules molecules)) 
				  grouped-ligands)
		successes (count-successes
			   flexible-dihedral?
			   charge-limit 
			   feature-scale steric-scale
			   optimiser success-rmsd grouped-ligands)
		success-rates (fmap #(int (* 100 (/ % (count grouped-ligands)))) 
				    successes)
		feature-counts (map #(count (apply concat (vals (find-features % charge-limit)))) molecules)]
	    [target-name 
	     (str (int (average (vals success-rates))))
	     (str (apply min (vals success-rates)))
	     (str (apply max (vals success-rates)))
	     (str (count grouped-ligands))
	     (format "%.2f" (double (average feature-counts)))
	     (str (apply min feature-counts))
	     (str (apply max feature-counts))])))))))
       
(def test-file-1 "data/grouped/flexs/carboxypth-a.mol2")
(def test-file-2 "data/grouped/flexs/concanavalin.mol2")
(def test-file-3 "data/grouped/flexs/g-phosphorylase.mol2")

(defn test-and-show []
  (align-and-show-table
   "--iterations" "10"
   "--rigid-molecule"
   test-file-2))