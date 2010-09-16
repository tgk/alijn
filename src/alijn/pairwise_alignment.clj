(ns alijn.pairwise-alignment
  (:use [alijn custom-features colored-point-alignment]))

(defn align 
  "Aligns a pair of molecules using their features.
  Returns variable-molecule shifted and rotated to."
  [threshold constant-molecule variable-molecule])