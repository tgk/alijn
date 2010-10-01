(ns alijn.cma-es
  (:use clojure.pprint)
  (:import [cma CMAEvolutionStrategy]))

(defn cma-es-minimise 
  [fun-evals
   objective-fn ranges]
  (let [cma (doto (CMAEvolutionStrategy. (count ranges))
	      (.readProperties "CMAEvolutionStrategy.properties")
	      (.setInitialX (double-array (map first ranges))
			    (double-array (map second ranges)))
	      ; This should probably be derived from the ranges
	      (.setInitialStandardDeviation 0.2))
	options (.options cma)
	parameters (.parameters cma)]
    (set! (.stopMaxFunEvals options) fun-evals)
    (set! (.stopFitness options) Double/NEGATIVE_INFINITY)
    (set! (.stopTolFun options) 1.0E-6)
    (set! (.stopTolFunHist options) 1.0E-7)
    (.init cma)
    (while (-> cma .stopConditions .isFalse)
	   (let [fitness (map (comp objective-fn seq) (.samplePopulation cma))]
	     (.updateDistribution cma (double-array fitness))))
    (.setFitnessOfMeanX cma (objective-fn (.getMeanX cma)))
    {:fun-evals (.getCountEval cma)
     :best (seq (.getBestX cma))}))

(defn repeatedly-cma-es-minimise
  [max-fun-evals
   fitness-logger
   objective-fn ranges]
  (apply 
   min-key 
   objective-fn
   (loop [fun-evals 0
	  solutions []]
     (when (and fitness-logger (seq solutions)) (fitness-logger fun-evals (- (apply min (map objective-fn solutions)))))
     (if (>= fun-evals max-fun-evals)
       solutions
       (let [{add-fun-evals :fun-evals, sol :best}
	     (cma-es-minimise (- max-fun-evals fun-evals) objective-fn ranges)]
	 (recur (+ fun-evals add-fun-evals) (conj solutions sol)))))))

(defn cma-es-optimiser 
  [fun-evals fitness-logger]
  (fn [objective-fn ranges]
    (repeatedly-cma-es-minimise 
     fun-evals 
     fitness-logger
     (memoize (comp - objective-fn))
     ranges)))