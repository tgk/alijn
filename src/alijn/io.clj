(ns alijn.io
  (:use clj-todo.todo)
  (:import 
   [java.io File FileInputStream FileOutputStream]
   [org.openscience.cdk DefaultChemObjectBuilder]
   [org.openscience.cdk.io SDFWriter]
   [org.openscience.cdk.io.iterator IteratingMDLReader]))

(defn read-sdf-file [filename]
  (let [file (File. filename)
        stream (FileInputStream. file)
        reader 
          (IteratingMDLReader. stream (DefaultChemObjectBuilder/getInstance))]
    (iterator-seq reader)))

(defn write-sdf-file [filename molecules]
  (let [file (File. filename)
	stream (FileOutputStream. file)
	writer (SDFWriter. stream)]
    (doseq [molecule molecules] (.write writer molecule))
    (.close writer)))

