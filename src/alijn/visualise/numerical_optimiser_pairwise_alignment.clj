(ns alijn.visualise.numerical-optimiser-pairwise-alignment
  (:use [alijn 
	 io 
	 differential-evolution 
	 numerical-optimiser-pairwise-alignment 
	 features 
	 molecule-visualisation 
	 utils]
	clojure.contrib.command-line))

(def desc 
"Performs pairwise alignment .")

(defn 
  #^{:doc desc}
  align-and-show
  [& args]
  (with-command-line args desc
    [filenames]
    (let [molecules (read-molecules-from-files filenames)
	  constant-molecule (first molecules)]
      (println (format "Parsed %d molecules." (count molecules)))
      (doseq [variable-molecule (rest molecules)]
	(let [charge-limit 0.5
	      optimiser (de-optimiser 50 0.5 0.75 10)
	      {moved-molecule :moved-molecule} (align charge-limit
						      1.0 0.5
						      optimiser constant-molecule variable-molecule)
	      grouped-features (find-features moved-molecule charge-limit)
	      grouped-features-points (extract-feature-points grouped-features)]
	  (show-molecules-app [constant-molecule moved-molecule] grouped-features-points))))))

(defn test-view [] 
  (align-and-show "data/grouped/small/carboxyptd-a.mol2"))