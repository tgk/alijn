(ns alijn.visualise.features
  (:use [alijn custom-features io molecule-visualisation utils]
	clojure.contrib.command-line))

(def desc 
"Identifies the features from the molecules in the files and opens
a view for each molecule containing the molecule and its features.")

(defn 
  #^{:doc desc}
  find-and-show-features 
  [& args]
  (with-command-line args desc
    [filenames]
    (let [molecules (read-molecules-from-files filenames)]
      (println (format "Parsed %d molecules." (count molecules)))
      (doseq [molecule molecules]
	(let [grouped-features (find-features molecule)
	      grouped-features-points (map-on-values (partial map get-point) 
						     grouped-features)
	      found-features (->> (for [[name centers] grouped-features-points]
				    (map vector (repeat name) centers))
				  (apply concat)
				  (apply concat))]
	  (show-molecules-app [molecule] found-features))))))

(defn test-view [] 
  (find-and-show-features "data/example/comt_ligands.mol2"))