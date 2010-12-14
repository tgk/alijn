(ns alijn.fitness)

(defprotocol Fitness
  (value [this] "Returns total value of fitness")
  (string-rep [this] "Returns a string representation of the fitness"))