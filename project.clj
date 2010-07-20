(defproject alijn "0.0.1-SNAPSHOT"
  :description "A program for extracting and aligning molecular pharmacophores."
  :repositories {"org.openscience" "https://maven.ch.cam.ac.uk/m2repo"}
  :dependencies [[org.clojure/clojure "1.2.0-master-SNAPSHOT"]
                 [org.clojure/clojure-contrib "1.2.0-SNAPSHOT"]
		 [org.openscience/cdk "1.3.4"]
		 [jama "1.0.1"]
		 [penumbra "0.6.0-SNAPSHOT"]]
  :native-dependencies [[penumbra/lwjgl "2.4.2"]]
  :dev-dependencies [
; Depends on clojure 1.1
		     [native-deps "1.0.1"]
		     [lein-run "1.0.0-SNAPSHOT"]
		     [clj-todo "0.1.0-SNAPSHOT"]
;		     [leiningen/lein-swank "1.1.0"]
;		     [swank-clojure "1.2.1"]
		     [swank-clojure "1.3.0-SNAPSHOT"]
		     
		     ]
; This fails under lein 1.2
;  :jvm-opts "-Xmx1g"
  :run-aliases {:align    [alijn.molecule-alignment perform-alignment]
		:features [alijn.visualise.features find-and-show-features]
		:read-molecules [alijn.read-mol-file read-and-print-names]}
)