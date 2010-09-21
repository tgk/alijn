(ns alijn.clique-detection
  (:use [bron-kerbosch]
	[clojure.contrib.combinatorics]
	[alijn math graph])
  (:require [alijn.utils :as utils]))

; Correspondance graph generator
(defn connect-graph
  "Connects node u to v when (pred u v) is true."
  [nodes pred]
  (let [basis (zipmap nodes (repeat []))]
    (apply
     merge-with concat basis
     (for [u nodes, v nodes :when (pred u v)] {u [v]}))))
  
(defn new-correspondance-graph 
  "Creates a correspondance graph from the two collections.
  Only nodes for which node-pred is true are made, and
  only pairs of nodes for which edge-pred is true are
  linked."
  [coll-1 coll-2 node-pred edge-pred]
  (let [nodes (for [a coll-1, u coll-2 :when (node-pred a u)] [a u])]
    (connect-graph nodes edge-pred)))

; Predicates
; Dummy predicate
(defn true-pred [& _] true)
; Node predicates
(defn same-color? [u v] (= (:color u) (:color v)))
; Edge predicates
(defn not-same-underlying-node?
  [[a u] [b v]]
  (and (not= a b) (not= u v)))
(comment defn not-same-underlying-colored-node?
  [[a u] [b v]]
  )

(defn same-distance? 
  [dist-1 dist-2]
  (fn [[a u] [b v]] (= (dist-1 a b) (dist-2 u v))))
(defn within? [threshold n m]
  (cond
   (= n m)   true
   (= nil n) false
   (= nil m) false
   :else     (<= (- threshold) (- m n) threshold)))
(defn within-distance? [threshold dist-1 dist-2]
  (fn [[a u] [b v]] 
    (within? threshold (dist-1 a b) (dist-2 u v))))
; Predicate combiner
(defn combine-predicates
  "Combine predicates."
  [& preds]
  (fn [& elms] (every? (fn [pred] (apply pred elms)) preds)))
; Helper for predicates on wrapped elements (stuff that's in a {:elm}
(defn wrapped-edge-predicate [pred]
  (fn [[u s] [v t]] (pred [(:elm u) (:elm s)] [(:elm v) (:elm t)])))

; Correspondance graphs from graphs
(defn correspondance-graph-from-graphs
  [edges-1 edges-2]
  (let [edge-pred 
	(combine-predicates 
	 not-same-underlying-node? 
	 (same-distance? (graph-distance-fn edges-1)
			 (graph-distance-fn edges-2)))]
    (new-correspondance-graph (keys edges-1) (keys edges-2) true-pred edge-pred)))

(defn correspondance-graph-from-colored-graphs
  [edges-1 edges-2]
  (let [edge-pred 
	(combine-predicates 
	 (wrapped-edge-predicate not-same-underlying-node?)
	 (same-distance? (graph-distance-fn edges-1)
			 (graph-distance-fn edges-2)))]
    (new-correspondance-graph (keys edges-1) (keys edges-2) same-color? edge-pred)))

; Correspondance graphs from points
(defn correspondance-graph-from-points
  [threshold points-1 points-2]
  (new-correspondance-graph 
   points-1 points-2 
   true-pred 
   (combine-predicates
    not-same-underlying-node? 
    (within-distance? threshold distance distance))))

(defn correspondance-graph-from-colored-points
  [threshold points-1 points-2]
  (new-correspondance-graph
   points-1 points-2
   same-color?
   (combine-predicates
    not-same-underlying-node?
    (wrapped-edge-predicate (within-distance? threshold distance distance)))))

; Possible pairings sub routine

(defn possible-pairings
  [correspondance-graph]
  (let [nodes (keys correspondance-graph)]
    (for [cliques (maximum-cliques nodes correspondance-graph)]
      [(map first cliques) (map second cliques)])))
