(ns alijn.visualise.pairwise-alignment
  (:use [alijn io custom-features pairwise-alignment molecule-visualisation]
	clojure.contrib.command-line))

(def desc 
"Reads the molecules from one or more files. Uses the first molecule
read as a reference and aligns the remaining molecules using features.")

(defn 
  #^{:doc desc}
  align-and-show
  [& args]
  (with-command-line args desc
    [[threshold "The threshold used by the clique detection algorithm." nil]
     filenames]
    (let [threshold (when threshold (Double/parseDouble threshold))
	  molecules (read-molecules-from-files filenames)
	  constant-molecule (first molecules)
	  constant-features (extract-feature-points (find-features constant-molecule))
	  variable-molecules molecules]
      (prn "Parsed" (count molecules) "molecules in total.")
      (prn "Aligning with respect to first.")
      (doseq [variable-molecule variable-molecules]
	(let [moved-variable (align threshold constant-molecule variable-molecule)]
	  (show-molecules-app [constant-molecule moved-variable]
			      constant-features))))))

(defn test-align-and-show []
  (align-and-show "-threshold" "2.0" 
		  "data/example/comt_ligands.mol2"))
  

(defn test-align-and-show-no-threshold []
  (align-and-show "data/example/comt_ligands.mol2"))
  