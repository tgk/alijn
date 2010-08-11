(defproject alijn "0.0.1-SNAPSHOT"
  :description "A program for extracting and aligning molecular pharmacophores."
  :repositories {"org.openscience" "https://maven.ch.cam.ac.uk/m2repo"}
  :dependencies [[org.clojure/clojure "1.2.0-master-SNAPSHOT"]
                 [org.clojure/clojure-contrib "1.2.0-SNAPSHOT"]
		 [org.openscience/cdk "1.3.5.git"]
		 [jama "1.0.1"]
		 [clj-todo "0.4.0-SNAPSHOT"]
		 [penumbra "0.6.0-SNAPSHOT"]]
  :native-dependencies [[penumbra/lwjgl "2.4.2"]]
  :dev-dependencies [[native-deps "1.0.1"]
		     [clj-todo "0.4.0-SNAPSHOT"]
		     [lein-run "1.0.0-SNAPSHOT"]
		     [swank-clojure "1.3.0-SNAPSHOT"]]
  :jvm-opts ["-Xmx1g"]
  :run-aliases {:align          [alijn.molecule-alignment perform-alignment]
		:features       [alijn.visualise.features find-and-show-features]
		:read-molecules [alijn.read-mol-file read-and-print-names]})