(ns alijn.cma-es
  (:use alijn.utils
        clojure.pprint
	clojure.contrib.logging)
  (:import [cma CMAEvolutionStrategy]))

(defn cma-es-minimise 
  [lambda
   max-evaluations
   objective-fn ranges]
  (let [cma (doto (CMAEvolutionStrategy. (count ranges))
	      (.readProperties "CMAEvolutionStrategy.properties")
	      (.setInitialX (double-array (map first ranges))
			    (double-array (map second ranges)))
	      ; This should probably be derived from the ranges
	      (.setInitialStandardDeviation 2.0))
	options (.options cma)
	parameters (.parameters cma)
	evaluations (atom 0)
	objective-fn (memoize-visible-atom 
		      (fn [xs] (swap! evaluations inc) (objective-fn xs)))]
    (.setPopulationSize parameters lambda)
    (set! (.stopFitness options) Double/NEGATIVE_INFINITY)
    (set! (.stopTolFun options) 1.0E-5)
    (set! (.stopTolFunHist options) 1.0E-6)
    (.init cma)
    (while (and (-> cma .stopConditions .isFalse)
		(< @evaluations max-evaluations))
	   (let [fitness (map (comp objective-fn seq) (.samplePopulation cma))]
	     (.updateDistribution cma (double-array fitness))))
    (.setFitnessOfMeanX cma (objective-fn (.getMeanX cma)))
    {:fun-evals @evaluations
     :best (seq (.getBestX cma))}))

(defn repeatedly-cma-es-minimise
  [lambda 
   max-fun-evals
   objective-fn ranges]
  (let [best (partial apply min-key objective-fn)]
    (loop [fun-evals 0
	   lambda lambda
	   solutions []]
      (comment when (> fun-evals 0) 
	(info (format "cma-es evaluations %d best-fitness %f" 
		      fun-evals (objective-fn (best solutions)))))
      (if (>= fun-evals max-fun-evals)
	(best solutions)
	(let [{add-fun-evals :fun-evals, sol :best}
	      (cma-es-minimise lambda (- max-fun-evals fun-evals) objective-fn ranges)]
	  (recur (+ fun-evals add-fun-evals) (* 2 lambda) (conj solutions sol)))))))

(defn cma-es-optimiser 
  [lambda fun-evals]
  (fn [objective-fn ranges]
    (repeatedly-cma-es-minimise 
     lambda
     fun-evals 
     (comp - objective-fn)
     ranges)))

(defn- example-ranges [n] 
  (for [i (range n)] [-1 1]))

(def evaluations (atom 0))
(defn- reset-evaluations []
  (swap! evaluations (constantly 0)))
(let [squared (fn [x] (* x x))]
  (defn- example-objective-fn [xs]
    (swap! evaluations inc)
    (reduce + (map squared xs))))