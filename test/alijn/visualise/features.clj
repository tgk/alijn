(ns alijn.visualise.features
  (:use [alijn features io molecule-visualisation]
	[clojure.contrib command-line]
	[clj-todo todo]))

(todo
 "This description isn't shown properly but only as a reference when used. Why is that?"
 (def desc 
      "Identifies the features from the molecules in the files and opens
a view for each molecule containing the molecule and its features.")
)

(defn find-and-show-features 
  desc
  [& args]
  (with-command-line args desc
    [[feature-file f "The feature file to use." nil]
     filenames]
    (if feature-file
      (let [features (parse-features feature-file)
	    molecules (read-molecules-from-files filenames)]
	(println (format "Parsed %d features and %d molecules." 
			 (count features) (count molecules)))
	(doseq [molecule molecules]
	  (let [grouped-features (feature-groups features molecule)
		found-features (->> (for [[name centers] grouped-features]
				      (map vector (repeat name) centers))
				    (apply concat)
				    (apply concat))]
	    (show-molecules-app [molecule] found-features))))
      (println "Must specify valid feature file."))))

(comment println "Description tests begin")
(comment find-and-show-features "--help")
(comment doc find-and-show-features)
(comment println "Description tests end")

(comment find-and-show-features 
	 "-f" "data/example/features.smarts"
	 "data/example/comt_subset.sdf")

