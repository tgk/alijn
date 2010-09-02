(ns alijn.io
  (:import 
   [java.io File FileInputStream FileOutputStream FileReader]
   [org.openscience.cdk ChemFile]
   [org.openscience.cdk.io SDFWriter ReaderFactory]
   [org.openscience.cdk.io.iterator IteratingMDLReader]
   [org.openscience.cdk.tools.manipulator ChemFileManipulator]))

(defn write-sdf-file [filename molecules]
  (let [file (File. filename)
	stream (FileOutputStream. file)
	writer (SDFWriter. stream)]
    (doseq [molecule molecules] (.write writer molecule))
    (.close writer)))

(defn read-molecules 
  "Reads all the molecules from a file. 
  Adapted from http://rguha.net/code/java/"
  [filename]
  (let [reader (->> filename
		    FileReader.
		    (.createReader (ReaderFactory.)))
	content (.read reader (new ChemFile))]
    (ChemFileManipulator/getAllAtomContainers content)))

(defn read-molecules-from-files [filenames]
  (apply concat (map read-molecules filenames)))