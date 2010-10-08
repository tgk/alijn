(defproject alijn "0.0.1-SNAPSHOT"
  :description "A program for extracting and aligning molecular pharmacophores."
   :repositories {"cs.au.dk" "http://cs.au.dk/~tgk/repo"}
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
		 [org.openscience/cdk "1.3.5.git"]
		 [jama "1.0.1"]
		 [bron-kerbosch "1.0.0-SNAPSHOT"]
		 [penumbra "0.6.0-SNAPSHOT"]
		 [cma-es "1.0"]
		 [log4j "1.2.15" :exclusions [javax.mail/mail
                                              javax.jms/jms
                                              com.sun.jdmk/jmxtools
                                              com.sun.jmx/jmxri]]]
  :native-dependencies [[penumbra/lwjgl "2.4.2"]]
  :dev-dependencies [[native-deps "1.0.4"]
		     [lein-run "1.0.0-SNAPSHOT"]
		     [swank-clojure "1.3.0-SNAPSHOT"]]
  :jvm-opts ["-Xmx1g"]
  :run-aliases {:features       [alijn.visualise.features find-and-show-features]
		:pairwise-align [alijn.visualise.pairwise-alignment align-and-show]
		:analyse-pairwise 
		[alijn.analyse.numerical-optimiser-pairwise-alignment 
		 align-and-show-table]
		:read-molecules [alijn.read-mol-file read-and-print-names]})