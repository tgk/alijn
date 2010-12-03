(ns alijn.analyse.recreate-binding-mode
  (:use [alijn optimisers io multiple-flexible-alignment molecule-utils logging]
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

(defn add-to-molecule-name! [molecule s]
  (let [new-name (str (molecule-name molecule) s)]
    (.setProperty molecule "cdk:Title" new-name)))

(defn
  #^{:doc desc}
  count-successes
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
     [success-rmsd "The maximum rmsd for a realignment to be a success." "2.5"]
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
	    (let [stationary-molecule (nth molecules i)
		  movable-molecules (concat (take i molecules) 
					    (drop (inc i) molecules))
		  log-filename (format "%s.%s.%d.%s.log"
				       optimiser
				       target-name run-number 
				       (molecule-name stationary-molecule))		  		  results (with-logger 
			    (file-logger log-filename)
			    (multiple-flexible-align 
			     stationary-molecule movable-molecules 
			     obj-fn-params 
			     optimiser-fn))
		  sdf-filename (format "%s.%s.%d.%s.sdf"
				       optimiser
				       target-name run-number 
				       (molecule-name stationary-molecule))
		  conformations (map :conformation results)]
	      (doseq [conf conformations] (add-to-molecule-name! conf "_conformation"))
	      (write-sdf-file sdf-filename (concat (cons stationary-molecule 
							 movable-molecules) 
						   conformations))
	      (println target-name 
		       (molecule-name stationary-molecule) 
		       (count-success (map :rmsd-to-native results)
				      (Double/parseDouble success-rmsd))))))))))