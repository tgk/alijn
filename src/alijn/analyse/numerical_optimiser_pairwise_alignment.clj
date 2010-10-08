(ns alijn.analyse.numerical-optimiser-pairwise-alignment
  (:use [alijn 
	 io 
	 features 
	 differential-evolution 
	 cma-es
	 numerical-optimiser-pairwise-alignment 
	 math 
	 molecule-utils 
	 utils
	 rotation-tree]
	clojure.contrib.command-line
	clojure.contrib.generic.functor
	clojure.contrib.profile
	clojure.pprint)
  (:import alijn.numerical-optimiser-pairwise-alignment.Objective-fn-params))

(def desc 
"Reads the molecules from one or more files. For each file, performs
pairwise alignment between every molecule in these and prints a 
table with the results.
Molecule names are truncated such that 1kcb_h_min becomes 1kcb.
If a file contains multiple molecules with the same name, the first
molecule is the native binding mode and the remaining are 
conformations. If only one molecule exists for each name they are
all native conformations that are sought re-aligned.
Possible optimisers are
 de-n-cr-f     (e.g. de-50-0.75-0.5)
 cma-es-lambda (e.g. cma-es-9)")

; Might move to common namespace
(defn parse-optimiser [fun-eval s]
  (let [parsers {"cma" (fn [_ lambda-str] 
			 (cma-es-optimiser (Integer/parseInt lambda-str)
					   fun-eval))
		 "de" (fn [n-str cr-str f-str]
			(de-optimiser (Integer/parseInt n-str)
				      (Double/parseDouble f-str)
				      (Double/parseDouble cr-str)
				      fun-eval))}
	tokens (.split s "-")]
    (apply (parsers (first tokens)) (rest tokens))))

(defn short-name [molecule] (first (.split (molecule-name molecule) "_")))  

(defn get-best-rmsd 
  [objective-fn-params
   optimiser constant-molecule [native-variable & variable-molecules]]
  (molecule-rmsd
   native-variable
   (:moved-molecule
    (align-with-multiple-variable 
      objective-fn-params
      optimiser 
      constant-molecule variable-molecules))))

(defn count-successes
  [objective-fn-params
   optimiser success-rmsd grouped-ligands]
  (fmap
   (fn [conformations]
     (let [reference (first conformations)
	   best-rmsds (map (partial get-best-rmsd
				    objective-fn-params
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
     [energy-contribution "What should the energy contribution be scaled with" "0.0"]
     [charge-limit "Limit for classifing atom as charged." "0.5"]
     [feature-scale "Scale to be used for Gaussian overlap" "1.0"]
     [steric-scale  "Scale to be used for Gaussian overlap" "0.5"]
     [success-rmsd "The maximum rmsd for a realignment to be a success." "2.5"]
     [fun-eval "Function evaluations" "100"]
     [optimiser "The optimiser to be used." "de-50-0.75-0.5"]
     filenames]
    (let [flexible-dihedral? (not rigid-molecule?)
	  objective-fn-params (Objective-fn-params. 
			       flexible-dihedral?
			       (Double/parseDouble energy-contribution)
			       (Double/parseDouble charge-limit)
			       (Double/parseDouble feature-scale)
			       (Double/parseDouble steric-scale))
	  success-rmsd (Double/parseDouble success-rmsd)
	  fun-eval (Integer/parseInt fun-eval)
	  optimiser (parse-optimiser fun-eval optimiser)]
      (println "Using flexible dihedral:" flexible-dihedral?)
      (print-table
       (cons
	["target" 
	 "avg. success" "min" "max" 
	 "ligands" 
	 "avg. features" "min" "max" 
	 "avg. dihedral" "min" "max"]
	(for [filename filenames]
	  (let [molecules (read-molecules filename)
		target-name (first (.split (last (.split filename "/")) "\\."))
		grouped-ligands (group-by short-name molecules)
		grouped-ligands (if (apply = 1 (vals (fmap count grouped-ligands))) 
				  (group-by short-name (concat molecules molecules)) 
				  grouped-ligands)
		successes (count-successes
			   objective-fn-params
			   optimiser success-rmsd grouped-ligands)
		success-rates (fmap #(int (* 100 (/ % (count grouped-ligands)))) 
				    successes)
		feature-counts (map 
				#(count 
				  (apply 
				   concat 
				   (vals 
				    (find-features 
				     % (:charge-limit objective-fn-params))))) 
				molecules)
		dihedral-angle-counts (map 
				       (comp :degrees-of-freedom 
					     calculate-rotation-tree) 
				       molecules)]
	    [target-name 
	     (str (int (average (vals success-rates))))
	     (str (apply min (vals success-rates)))
	     (str (apply max (vals success-rates)))
	     (str (count grouped-ligands))
	     (format "%.2f" (double (average feature-counts)))
	     (str (apply min feature-counts))
	     (str (apply max feature-counts))
	     (format "%.2f" (double (average dihedral-angle-counts)))
	     (str (apply min dihedral-angle-counts))
	     (str (apply max dihedral-angle-counts))])))))))
       
(def test-file-1 "data/grouped/flexs/carboxypth-a.mol2")
(def test-file-2 "data/grouped/flexs/concanavalin.mol2")
(def test-file-3 "data/grouped/flexs/g-phosphorylase.mol2")

(defn test-and-show []
  (align-and-show-table
   "--fun-eval" "10"
   "--optimiser" "de-10-0.7-0.5"
   ;"--optimiser" "cma-es-9"
   "--rigid-molecule"
   test-file-2))