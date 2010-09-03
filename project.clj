(defproject alijn "0.0.1-SNAPSHOT"
  :description "A program for extracting and aligning molecular pharmacophores."
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
		 [org.openscience/cdk "1.3.5.git"]
		 [jama "1.0.1"]
		 [bron-kerbosch "1.0.0-SNAPSHOT"]
		 [penumbra "0.6.0-SNAPSHOT"]]
  :native-dependencies [[penumbra/lwjgl "2.4.2"]]
  :dev-dependencies [[native-deps "1.0.4"]
		     [lein-run "1.0.0-SNAPSHOT"]
		     [swank-clojure "1.3.0-SNAPSHOT"]]
  :jvm-opts ["-Xmx1g"]
  :run-aliases {:align          [alijn.molecule-alignment perform-alignment]
		:test-align     [alijn.molecule-alignment perform-alignment 
				 "--samples" "1"
				 "--feature-file" "data/example/phase_supplement.smarts"
				 "--phase-file" "data/example/phase_smarts.def"
				 "data/example/comt_ligands.mol2"]
		:features       [alijn.visualise.features find-and-show-features]
		:read-molecules [alijn.read-mol-file read-and-print-names]})