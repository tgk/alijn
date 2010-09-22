(ns alijn.analyse.pairwise-alignment
  (:use [alijn io features pairwise-alignment math utils]
	clojure.contrib.command-line))

(defn get-rmsd-of-optimal-alignment
  [threshold charge-limit constant-molecule variable-molecule]
  (molecule-rmsd 
   variable-molecule
   (align threshold constant-molecule variable-molecule charge-limit)))

(def desc 
"Reads the molecules from one or more files. For each file, performs
pairwise alignment between every molecule in these and prints a 
table with the results.")

(defn 
  #^{:doc desc}
  align-and-show-table
  [& args]
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
	["target" "avg. success" "min" "max" "# ligands"]
	(for [filename filenames
	      :let [molecules (read-molecules filename)
		    name (first (.split (last (.split filename "/")) "\\."))
		    rmsds (for [molecule molecules]
			    (map (partial 
				  get-rmsd-of-optimal-alignment 
				  threshold charge-limit molecule) 
				 molecules))
		    successes (for [l rmsds] (map #(<= % success-rmsd) l))
		    rates (for [l successes] (/ (count (filter true? l)) (count l)))
		    avg (str (int (* 100 (average rates))))
		    min_succ (str (int (* 100 (apply min rates))))
		    max_succ (str (int (* 100 (apply max rates))))
		    ]]
	  [name avg min_succ max_succ (str (count molecules))]))))))
  
(def test-file-1 "data/example/carboxypth-a.mol2")
(def test-file-2 "data/example/concanavalin.mol2")
(def test-file-3 "data/grouped/flexs/g-phosphorylase.mol2")

(defn test-align-and-show []
  (align-and-show-table
   "--threshold" "1" 
   test-file-3))