(ns alijn.analyse.pharmacophore-alignment
  (:use [alijn io logging optimisers pharmacophore-alignment molecule-utils]
	clojure.contrib.command-line))

(def desc 
     (str
"Examines if the native binding mode of a set of molecules can
be recreated using multiple flexible alignment.
Each input molecule file corresponds to a target, and the
ligands in each file are assumed to be in their native 
binding mode.
"
optimiser-help))

(defn
  #^{:doc desc}
  align
  [& args]
  (with-command-line args desc
    [[optimiser "Optimiser to use" "de-50-0.75-0.5"]
     [fun-eval "Function evaluations" "10000"]
     [energy-contribution "What should the energy contribution be scaled with" "0.01"]
     [charge-limit "Limit for classifing atom as charged." "0.5"]
     [feature-scale "Scale to be used for Gaussian overlap" "1.0"]
     [steric-scale  "Scale to be used for Gaussian overlap" "0.5"]
     [fun-eval "Function evaluations" "10000"]
     [optimiser "The optimiser to be used." "de-50-0.75-0.5"]
     filenames]
    (let [obj-fn-params {:energy-contribution (Double/parseDouble energy-contribution)
			 :charge-limit (Double/parseDouble charge-limit)
			 :feature-scale (Double/parseDouble feature-scale)
			 :steric-scale (Double/parseDouble steric-scale)}
	  optimiser-fn (parse-optimiser (Integer/parseInt fun-eval) optimiser)
	  run-number (rand-int 10000)]
      (println "run-number " run-number " (used for filenames)")
      (doseq [filename filenames]
	(let [target-name (first (.split (last (.split filename "/")) "\\."))
	      molecules (read-molecules filename)]
	  (dotimes [i (count molecules)]
	    (let [molecule (nth molecules i)
		  pharmacophore-molecules (concat (take i molecules) 
						  (drop (inc i) molecules))
		  log-filename (format "pharmacophore-alignment.%s.%s.%d.%s.log"
				       optimiser
				       target-name run-number
				       (molecule-name molecule))
 		  result (with-logger 
			   (file-logger log-filename)
			   (align-molecule-to-pharmacophore
			    pharmacophore-molecules
			    molecule
			    optimiser-fn
			    obj-fn-params))
		  sdf-filename (format "pharmacophore-alignment.%s.%s.%d.%s.sdf"
				       optimiser
				       target-name run-number 
				       (molecule-name molecule))
		  conformation (:conformation result)]
	      (add-to-molecule-name! conformation "_conformation")
	      (write-sdf-file sdf-filename (cons conformation molecules))
	      (println target-name 
		       (molecule-name molecule) 
		       "rmsd"
		       (:rmsd-to-native result)
		       "fitness"
		       (:fitness result)))))))))