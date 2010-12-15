(ns alijn.analyse.recreate-binding-mode
  (:use [alijn 
	 io multiple-flexible-alignment 
	 molecule-utils logging features optimisers]
	alijn.analyse.standard-parameters
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

(defn count-success [rmsds success-rmsd]
  (let [n (count rmsds)
	success? (partial > success-rmsd)
	successes (count (filter success? rmsds))
	successes-without-stationary (count (filter success? (rest rmsds)))]
  (format "%.0f (%.0f)" 
	  (* 100 (float (/ successes n)))
	  (* 100 (float (/ successes-without-stationary (dec n)))))))

(defn
  #^{:doc desc}
  count-successes
  [& args]
  (with-standard-parameters
    energy-contribution charge-limit feature-scale steric-scale
    obj-fn-params optimiser optimiser-fn fun-eval 
    args desc 
    [[success-rmsd "The maximum rmsd for a realignment to be a success." "2.5"]
     filenames]
    (let [run-number (rand-int 10000)]
      (println "run-number " run-number " (used for filenames)")
      (doseq [filename filenames]
	(let [target-name (first (.split (last (.split filename "/")) "\\."))
	      molecules (read-molecules filename)]
	  (dotimes [i (count molecules)]
	    (let [stationary-molecule (nth molecules i)
		  movable-molecules (concat (take i molecules) 
					    (drop (inc i) molecules))
		  log-filename (format "recreate.%s.%s.%d.%s.log"
				       optimiser
				       target-name run-number 
				       (molecule-name stationary-molecule))		  		  results (with-logger 
			    (file-logger log-filename)
			    (multiple-flexible-align 
			     stationary-molecule movable-molecules 
			     obj-fn-params 
			     optimiser-fn))
		  sdf-filename (format "recreate.%s.%s.%d.%s.sdf"
				       optimiser
				       target-name run-number 
				       (molecule-name stationary-molecule))
		  conformations (map :conformation results)]
	      (doseq [conf conformations] (add-to-molecule-name! conf "_conformation"))
	      (write-sdf-file sdf-filename 
			      (concat (cons stationary-molecule 
					    movable-molecules) 
				      conformations
				      (molecules-from-features 
				       (apply 
					merge-with concat
					(map #(extract-feature-points
					       (find-features 
						% (:charge-limit obj-fn-params)))
					     conformations)))))
	      (write-sdf-file (format "%s.%s.%d.%s.phamacophore-model.sdf" 
				      optimiser target-name run-number
				      (molecule-name stationary-molecule))
			      conformations)
	      (println target-name 
		       (molecule-name stationary-molecule) 
		       (count-success (map :rmsd-to-native results)
				      (Double/parseDouble success-rmsd))))))))))