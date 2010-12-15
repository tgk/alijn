(ns alijn.analyse.alternating-aligner
  (:use [alijn 
	 io optimisers alternating-aligner 
	 molecule-utils logging features]
	alijn.analyse.standard-parameters))

(def desc 
     (str
"Examines if the native binding mode of a set of molecules can
be recreated using multiple flexible alignment where the
strategy is alternated between all molecules (except one)
being able to move around space, and only one molecule being moved.
The method will spread the number of function evaluations out
according to the following scheme:
1/3 for a multiple flexible alignment
1/3 for a local alignment
1/3 for a multiple flexible alignment
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
  align
  [& args]
  (with-standard-parameters
    energy-contribution charge-limit feature-parameters steric-scale
    obj-fn-params optimiser optimiser-fn
    fun-eval 
    args desc 
    [[success-rmsd "The maximum rmsd for a realignment to be a success." "2.5"]
     filenames]
    (let [run-number (rand-int 10000)]
      (println "run-number" run-number)
      (doseq [filename filenames]
	(let [target-name (first (.split (last (.split filename "/")) "\\."))
	      molecules (read-molecules filename)
	      pre-fun-evals (int (/ fun-eval 3))
	      local-fun-evals (int (/ (/ fun-eval 3) (dec (count molecules))))
	      post-fun-evals (- fun-eval pre-fun-evals 
				(* (dec (count molecules)) local-fun-evals))
	      pre-optimiser (parse-optimiser pre-fun-evals optimiser)
	      middle-optimiser (parse-optimiser local-fun-evals optimiser)
	      post-optimiser (parse-optimiser post-fun-evals optimiser)]
	  (dotimes [i (count molecules)]
	    (let [stationary-molecule (nth molecules i)
		  movable-molecules (concat (take i molecules) 
					    (drop (inc i) molecules))
		  log-filename (format "alternating.%s.%s.%d.%s.log"
				       optimiser
				       target-name run-number 
				       (molecule-name stationary-molecule))
		  results (with-logger 
			    (file-logger log-filename)
			    (alternating-align
			     stationary-molecule
			     movable-molecules obj-fn-params 
			     pre-optimiser middle-optimiser post-optimiser))
		  sdf-filename (format "alternating.%s.%s.%d.%s.sdf"
				       optimiser
				       target-name run-number 
				       (molecule-name stationary-molecule))
		  conformations (map :conformation results)]
	      (doseq [conf conformations] (add-to-molecule-name! conf "_conformation"))
	      (write-sdf-file sdf-filename 
			      (concat (cons stationary-molecule 
					    movable-molecules) 
				      conformations
;				      (comment molecules-from-features 
;					       (apply 
;					merge-with concat
;					(map #(extract-feature-points
;					       (find-features 
;						% (:charge-limit obj-fn-params)))
;					     conformations)))
				      ))
	      (write-sdf-file (format "alternating.%s.%s.%d.%s.phamacophore-model.sdf" 
				      optimiser target-name run-number
				      (molecule-name stationary-molecule))
			      conformations)
	      (println target-name 
		       (molecule-name stationary-molecule) 
		       (count-success (map :rmsd-to-native results)
				      (Double/parseDouble success-rmsd))))))))))