(defproject alijn "0.0.1-SNAPSHOT"
  :description "A program for extracting and aligning molecular pharmacophores."
  :repositories {"org.openscience" "https://maven.ch.cam.ac.uk/m2repo"}
  :dependencies [[org.clojure/clojure "1.1.0"]
                 [org.clojure/clojure-contrib "1.1.0"]
		 [org.openscience/cdk "1.3.4"]
		 [jama "1.0.1"]
		 [penumbra "0.5.0"]]
  :native-dependencies [[lwjgl "2.2.2"]]
  :dev-dependencies [[lein-run "1.0.0-SNAPSHOT"]
		     [swank-clojure "1.2.0"]
		     [clj-todo "0.1.0-SNAPSHOT"]
		     [native-deps "1.0.0"]]
  :namespaces [alijn.molecule-alignment]
  :main alijn.molecule-alignment
  :jvm-opts "-Xmx1g"
  :run-aliases {:align [alijn.molecule-alignment perform-alignment]}
)