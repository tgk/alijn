(ns alijn.cma-es
  (:use clojure.pprint)
  (:import [cma CMAEvolutionStrategy]))

(defn cma-es-minimise 
  [fun-evals
   objective-fn ranges]
  (let [cma (doto (CMAEvolutionStrategy.)
	      (.readProperties "CMAEvolutionStrategy.properties")
	      (.setDimension (count ranges))
	      (.setInitialX (double-array (map first ranges))
			    (double-array (map second ranges)))
	      ; This should probably be derived from the ranges
	      (.setInitialStandardDeviation 0.2))
	options (.options cma)]
    (set! (.stopMaxFunEvals options) fun-evals)
    (.init cma)
    (while (-> cma .stopConditions .isFalse)
	   (let [fitness (map (comp objective-fn seq) (.samplePopulation cma))]
	     (.updateDistribution cma (double-array fitness))))
    (.setFitnessOfMeanX cma (objective-fn (.getMeanX cma)))
    {:fun-evals (.getCountEval cma)
     :best (seq (.getBestX cma))}))

(defn repeatedly-cma-es-minimise
  [max-fun-evals
   objective-fn ranges]
  (apply 
   min-key 
   objective-fn
   (loop [fun-evals 0
	  solutions []]
     (if (>= fun-evals max-fun-evals)
       solutions
       (let [{add-fun-evals :fun-evals, sol :best}
	     (cma-es-minimise (- max-fun-evals fun-evals) objective-fn ranges)]
	 (recur (+ fun-evals add-fun-evals) (conj solutions sol)))))))

(defn cma-es-optimiser 
  [fun-evals]
  (fn [objective-fn ranges]
    (repeatedly-cma-es-minimise 
     fun-evals 
     #(- 10000 (objective-fn %)) 
     ;(comp - objective-fn) 
     ranges)))