(ns alijn.cma-es
  (:use [alijn utils logging fitness]
        clojure.pprint)
  (:import [cma CMAEvolutionStrategy]))

(defn cma-es-minimise 
  [lambda
   max-evaluations
   fitness-fn ranges]
  (let [cma (doto (CMAEvolutionStrategy. (count ranges))
	      (.readProperties "CMAEvolutionStrategy.properties")
	      (.setInitialX (double-array (map first ranges))
			    (double-array (map second ranges)))
	      (.setInitialStandardDeviation (apply 
					     min
					     (map 
					      (fn [[lo hi]] 
						(Math/abs (/ (- hi lo) 4)))
					      ranges))))
	options (.options cma)
	parameters (.parameters cma)
	evaluations (atom 0)
	
	fitness-fn (fn [xs] (swap! evaluations inc) (fitness-fn xs))]
    (.setPopulationSize parameters lambda)
    (set! (.stopFitness options) Double/NEGATIVE_INFINITY)
    (set! (.stopTolFun options) 1.0E-5)
    (set! (.stopTolFunHist options) 1.0E-6)
    (.init cma)
    (while (and (-> cma .stopConditions .isFalse)
		(< @evaluations max-evaluations))
	   (let [pop (.samplePopulation cma)
		 fitness (map (comp fitness-fn seq) pop)
		 objectives (map (comp - value) fitness)]
	     (log-fitness "CMA-ES" @evaluations 
			  (apply max-key value fitness))
	     (.updateDistribution cma (double-array objectives))))
    (.setFitnessOfMeanX cma (- (value (fitness-fn (seq (.getMeanX cma))))))
    {:fun-evals @evaluations
     :best (seq (.getBestX cma))}))

(defn repeatedly-cma-es-minimise
  [lambda 
   max-fun-evals
   fitness-fn ranges]
  (let [;objective-fn (comp - value fitness-fn)
	best (partial apply min-key (comp - value fitness-fn))]
    (loop [fun-evals 0
	   lambda lambda
	   solutions []]
      (if (>= fun-evals max-fun-evals)
	(best solutions)
	(let [{add-fun-evals :fun-evals, sol :best}
	      (cma-es-minimise lambda (- max-fun-evals fun-evals) 
			       fitness-fn ranges)]
	  (recur (+ fun-evals add-fun-evals) (* 2 lambda) (conj solutions sol)))))))

(defn cma-es-optimiser 
  [lambda fun-evals]
  (fn [fitness-fn ranges]
    (repeatedly-cma-es-minimise 
     lambda
     fun-evals 
     fitness-fn
     ;(comp - objective-fn)
     ranges)))