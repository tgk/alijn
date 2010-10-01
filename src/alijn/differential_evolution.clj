(ns alijn.differential-evolution)

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
   fun-evals fitness-logger]
  (let [iterations (/ fun-evals n)
	objective-fn (memoize objective-fn)
	dim (count ranges)]
    (loop [iteration 0
	   population (initialise-population ranges n)]
      (when fitness-logger (fitness-logger (* iteration n) (apply max (map objective-fn population))))
      (if (>= iteration iterations)
	(apply max-key objective-fn population)
	(let [next-generation (map
			       (partial create-offspring dim scaling-factor crossover-rate population)
			       population)]
	  (recur 
	   (inc iteration) 
	   (map (partial max-key objective-fn) population next-generation)))))))

(defn de-optimiser [n scaling-factor crossover-rate fun-evals fitness-logger]
  (fn [objective-fn ranges] 
    (find-max objective-fn ranges 
	      n scaling-factor crossover-rate 
	      fun-evals fitness-logger)))