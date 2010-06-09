(defproject alijn "0.0.1-SNAPSHOT"
  :description "A program for extracting and aligning molecular pharmacophores."
  :repositories {"org.openscience" "https://maven.ch.cam.ac.uk/m2repo"}
  :dependencies [[org.clojure/clojure "1.1.0"]
                 [org.clojure/clojure-contrib "1.1.0"]
		 [org.openscience/cdk "1.3.4"]
		 [jama "1.0.1"]]
  :dev-dependencies [[leiningen-run "0.2"]
		     [swank-clojure "1.2.1"]
		     [clj-todo "0.1.0-SNAPSHOT"]])