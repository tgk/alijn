(ns alijn.test-data
  (:use [alijn io features]))

(defn concanavalin []
  (read-molecules "data/grouped/flexs/concanavalin.mol2"))

(defn carboxy []
  (read-molecules "data/grouped/flexs/carboxyptd-a.mol2"))

(def standard-feature-parameters
     (parse-feature-parameters "feature.parameters"))