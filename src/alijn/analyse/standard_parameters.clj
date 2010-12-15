(ns alijn.analyse.standard-parameters
  (:use [alijn optimisers features]
	clojure.contrib.command-line
	clojure.pprint))

(defmacro with-standard-parameters
  "Binds the standard parameters to obj-fn-params, optimiser, 
  optimiser-fn and fun-eval.
  Additional cmdspec are as in with-command-line.
  cmspec definitions can not use energy-contribution, charge-limit, 
  feature-scale, steric-scale, fun-eval or optimiser."
  [energy-contribution charge-limit feature-parameters steric-scale 
   obj-fn-params optimiser optimiser-fn fun-eval
   args desc cmdspec body]
  `(with-command-line
     ~args
     ~desc
     ~(concat [[energy-contribution "Energy contribution scale" "10"]
	       [charge-limit "Limit for classifing atom as charged." "0.5"]
	       [feature-parameters "Feature parameters file" 
		"feature.parameters"]
	       [steric-scale  "Scale to be used for Gaussian overlap" "0.5"]
	       [fun-eval "Function evaluations" "10000"]
	       [optimiser "The optimiser to be used." "de-50-0.75-0.5"]]
	      cmdspec)
     (let [~obj-fn-params {:energy-contribution 
			   (Double/parseDouble ~energy-contribution)
			   :charge-limit (Double/parseDouble ~charge-limit)
			   :feature-parameters 
			   (parse-feature-parameters ~feature-parameters)
			   :steric-scale (Double/parseDouble ~steric-scale)}
	   ~fun-eval (Integer/parseInt ~fun-eval)
	   ~optimiser-fn (parse-optimiser ~fun-eval ~optimiser)]
       ~body)))

(comment
(pprint (with-standard-parameters
	  ec charge-limit fs ss
	  foo bar bar-fn baz
	  ["--charge-limit" "42.0" "foobar.mol2" "baz.mol2"]
	  "Foo"
	  [[success-rmsd "When rmsd to native is below this, it is a success." "2.5"]
	   filenames]
	  (cons foo filenames)))
)