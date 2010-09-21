(ns alijn.analyse.pairwise-alignment
  (:use [alijn io custom-features pairwise-alignment math utils]
	clojure.contrib.command-line))

(defn get-rmsd-of-optimal-alignment
  [threshold constant-molecule variable-molecule]
  (molecule-rmsd 
   variable-molecule
   (align threshold constant-molecule variable-molecule)))

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
     [success-rmsd "The maximum rmsd for a realignment to be a success." 2.5]
     filenames]
    (let [threshold (when threshold (Double/parseDouble threshold))]
      (print-table
       (cons
	["target" "avg. success rate" "# ligands"]
	(for [filename filenames
	      :let [molecules (read-molecules filename)
		    name (first (.split (last (.split filename "/")) "\\."))
		    rmsds (for [molecule molecules]
			    (map (partial 
				  get-rmsd-of-optimal-alignment 
				  threshold molecule) 
				 molecules))
		    successes (for [l rmsds] (map #(<= % success-rmsd) l))
		    rates (for [l successes] (/ (count (filter true? l)) (count l)))
		    avg (str (* 100 (average rates)))]]
	  [name avg (str (count molecules))]))))))
  
(def test-file-1 "data/example/carboxypth-a.mol2")
(def test-file-2 "data/example/concanavalin.mol2")


(defn test-align-and-show []
  (align-and-show-table
   "--threshold" "0.1" 
   test-file-1 test-file-2))

(defn test-align-and-show-no-threshold []
  (align-and-show-table test-file-1))