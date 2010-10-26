(ns alijn.differential-evolution
  (:use alijn.utils
	clojure.contrib.logging))

(defn initialise-population [ranges n]
  (for [i (range n)]
    (for [[range-min range-max] ranges]
      (+ range-min (rand (- range-max range-min))))))

(defn create-offspring [dim scaling-factor crossover-rate population parent]
  (let [p1 (rand-nth population)
	p2 (rand-nth population)
	p3 (rand-nth population)
	mutate? (set (for [i (range dim) :when (<= (rand) crossover-rate)] i))]
    (for [i (range dim)] 
      (if (mutate? i)
	(+ (nth p1 i) (* scaling-factor (- (nth p2 i) (nth p3 i))))
	(nth parent i)))))

(defn find-max
  "Performs a DE optimisation finding a vector of numbers
  where the objective-fn takes on a high value."
  [objective-fn
   ranges
   n scaling-factor crossover-rate
   fun-evals]
  (let [objective-fn (memoize-visible-atom objective-fn)
	iterations (/ fun-evals n)
	best (partial apply max-key objective-fn)
	dim (count ranges)]
    (loop [iteration 0
	   population (initialise-population ranges n)]
      (info (format "de evaluations %d best-fitness %f" 
		    (* iterations n)
		    (best population)))
      (when (= 0 (mod iterations 100)) (reset-mem! objective-fn))
      (if (>= iteration iterations)
	(best population)
	(let [next-generation (map
			       (partial create-offspring dim 
					scaling-factor crossover-rate 
					population)
			       population)]
	  (recur 
	   (inc iteration) 
	   (map (partial max-key objective-fn) population next-generation)))))))

(defn de-optimiser [n scaling-factor crossover-rate fun-evals]
  (fn [objective-fn ranges] 
    (find-max objective-fn ranges 
	      n scaling-factor crossover-rate 
	      fun-evals)))