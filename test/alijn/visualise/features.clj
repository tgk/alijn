(ns alijn.visualise.features
  (:use [alijn features io molecule-visualisation]
	[clojure.contrib command-line]))

(def desc 
"Identifies the features from the molecules in the files and opens
a view for each molecule containing the molecule and its features.")

(defn 
  #^{:doc desc}
  find-and-show-features 
  [& args]
  (with-command-line args desc
    [[feature-file f "A feature file." nil]
     [phase-file p "A phase feature definition file." nil]
     filenames]
    (let [custom-features (if feature-file (parse-features feature-file) {})
	  phase-features (if phase-file (parse-phase-features phase-file) {})
	  features (merge-with concat custom-features phase-features)
	  molecules (read-molecules-from-files filenames)]
      (println (format "Parsed %d features and %d molecules." 
		       (count features) (count molecules)))
      (doseq [molecule molecules]
	(let [grouped-features (feature-groups features molecule)
	      found-features (->> (for [[name centers] grouped-features]
				    (map vector (repeat name) centers))
				  (apply concat)
				  (apply concat))]
	  (show-molecules-app [molecule] found-features))))))

(comment
  find-and-show-features 
  "-f" "data/example/phase.smarts"
  "-p" "data/example/phase_smarts.def"
  "data/example/comt_subset.sdf")