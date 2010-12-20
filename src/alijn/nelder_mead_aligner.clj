(ns alijn.nelder-mead-aligner
  (:use [alijn 
	 molecule-manipulation rotation-tree 
	 conformation objective
	 nelder-mead
	 logging fitness
	 math])
  (:import javax.vecmath.Point3d))

(defn randomise-molecules
  "Randomises the translation, orientation and configuration
  of a set of a molecules.
  The first of the molecules is not moved and rotated."
  [molecules]
  (let [trees (map calculate-rotation-tree molecules)
	random-confs (map random-configuration trees)]
    (cons (first random-confs)
	  (map (comp randomise-molecule-orientation random-translation)
	       (rest random-confs)))))

(defn rand-float [lo hi] (+ (* (- hi lo) (rand)) lo))

(defn shift-translations [molecules]
  (cons
   (first molecules)
   (for [molecule (rest molecules)]
     (move-molecule-center 
      molecule 
      (vec-add
       (center-of-mass molecule)
       (Point3d. (rand-float -0.01 0.01) 
		 (rand-float -0.01 0.01) 
		 (rand-float -0.01 0.01)))))))

(defrecord DummyFitness [fitness]
  Fitness
  (value [this] fitness)
  (string-rep [this] (str fitness)))

(defn nelder-mead-align [molecules max-iter max-iter-pr-point 
			 objective-fn-params]
  (let [randomised-molecules (randomise-molecules molecules)
	center (center-of-mass (first randomised-molecules))
	centered-molecules (map 
			    #(move-molecule-center % center) 
			    randomised-molecules)]
    (loop [iters 0
	   best-configuration :no-configuration
	   best-value :no-value]
      (let [random-orientation (map 
				randomise-molecule-orientation 
				centered-molecules)
	    shifted-translations (shift-translations random-orientation)
	    {conformations :conformations
	     ranges :ranges} (conformation-fn 
			      (first shifted-translations)
			      (rest shifted-translations))
	    start (repeat (count ranges) 0.0)
	    start (repeatedly (count ranges) (fn [] (* 0.0001 (rand))))
	    step-sizes (repeat (count ranges) 0.5)
	    obj-fn (objective-fn shifted-translations objective-fn-params)
	    fitness (comp - :total obj-fn conformations)
	    [minimum min-vector used-iter] 
	    (nm-minimise fitness start step-sizes 1e-4 
			 (min (- max-iter iters) max-iter-pr-point))
	    value (- minimum)
	    iters (+ iters used-iter)
	    [best-configuration best-value] (if (or (= best-value :no-value) 
						    (> value best-value))
					      [(conformations min-vector)
					       value]
					      [best-configuration
					       best-value])]
	(log-fitness "NM" iters (DummyFitness. best-value))
	(if (>= iters max-iter)
	  [best-configuration best-value]
	  (recur iters best-configuration best-value))))))