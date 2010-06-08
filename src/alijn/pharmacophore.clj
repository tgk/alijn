(ns alijn.pharmacophore
  (:import 
   [org.openscience.cdk.smiles SmilesParser SmilesGenerator]))

; Taken from 
; http://www.daylight.com/dayhtml_tutorials/languages/smarts/smarts_examples.html
; More advanced definitions available from same site.
(def example-pharmacophores
     {"hydrogen-bond acceptor" "[#6,#7;R0]=[#8]"
      "hydrogen-bond donor" "[!$([#6,H0,-,-2,-3])]"
      "aromatic-5-ring" "[oX2r5]"})

