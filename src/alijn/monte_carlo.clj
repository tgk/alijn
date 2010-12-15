(ns alijn.monte-carlo
  (:use alijn.fitness))

(defn sample [ranges]
  (map 
   (fn [[lo hi]] (+ (rand (- hi lo)) lo))
   ranges))

(defn monte-carlo-optimiser
  [max-fun-evals]
  (fn [fitness-fn ranges]
    (apply 
     max-key (comp value fitness-fn)
     (repeatedly max-fun-evals (partial sample ranges)))))