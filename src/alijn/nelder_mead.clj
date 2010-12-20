(ns alijn.nelder-mead
  (:use alijn.fitness)
  (:import [flanagan.math MinimisationFunction Minimisation]))

(defn- nm-wrap-fn [f]
  (proxy [MinimisationFunction] []
      (function [xs] (f (seq xs)))))

(defn nm-minimise [f start step-sizes conv-tol max-iter]
  (let [minimiser (Minimisation.)]
    (.setNmax minimiser max-iter)
    (.nelderMead 
     minimiser 
     (nm-wrap-fn f)
     (double-array start) (double-array step-sizes)
     conv-tol)
    [(.getMinimum minimiser)
     (seq (.getParamValues minimiser))
     (.getNiter minimiser)]))