(ns alijn.visualise.features
  (:use [alijn features io molecule-visualisation]
	clojure.contrib.command-line))

(def desc 
"Identifies the features from the molecules in the files and opens
a view for each molecule containing the molecule and its features.")

(defn 
  #^{:doc desc}
  find-and-show-features 
  [& args]
  (with-command-line args desc
    [[phase-file p "A phase feature definition file."]
     filenames]
    (let [phase-features (parse-phase-with-tags phase-file)
	  molecules (read-molecules-from-files filenames)]
      (println (format "Parsed %d features and %d molecules." 
		       (count phase-features) (count molecules)))
      (doseq [molecule molecules]
	(let [grouped-features (new-find-all-features molecule phase-features)
	      found-features (->> (for [[name centers] grouped-features]
				    (map vector (repeat name) centers))
				  (apply concat)
				  (apply concat))]
	  (show-molecules-app [molecule] found-features))))))

(defn test-view [] 
  (find-and-show-features "-p" "data/example/custom_smarts.def"
			  "data/example/comt_ligands.mol2"))

