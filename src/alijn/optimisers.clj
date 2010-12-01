(ns alijn.optimisers
  (:use [alijn differential-evolution cma-es monte-carlo]))

(def optimiser-help
"Optimisers are one of
 de-n-cr-f     (e.g. de-50-0.75-0.5)
 cma-es-lambda (e.g. cma-es-18)
 monte-carlo")

(defn 
  #^{:doc optimiser-help}
  parse-optimiser 
  [fun-eval s]
  (let [parsers {"cma" (fn [_ lambda-str] 
			 (cma-es-optimiser (Integer/parseInt lambda-str)
					   fun-eval))
		 "de" (fn [n-str cr-str f-str]
			(de-optimiser (Integer/parseInt n-str)
				      (Double/parseDouble f-str)
				      (Double/parseDouble cr-str)
				      fun-eval))
		 "monte" (fn [_] (monte-carlo-optimiser fun-eval))}
	tokens (.split s "-")]
    (apply (parsers (first tokens)) (rest tokens))))