(ns alijn.monte-carlo)

(defn sample [ranges]
  (map 
   (fn [[lo hi]] (+ (rand (- hi lo)) lo))
   ranges))

(defn monte-carlo-optimiser
  [max-fun-evals]
  (fn [objective-fn ranges]
    (apply 
     max-key objective-fn 
     (repeatedly max-fun-evals (partial sample ranges)))))