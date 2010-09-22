(ns alijn.graph
  (:use clojure.contrib.generic.functor alijn.utils))

(defn same-graph? [graph-1 graph-2]
  "Only works if nodes are exactly the same and graphs are edge maps."
  (= (fmap set graph-1)
     (fmap set graph-2)))

(defn undirected-graph 
  "Creates an undirected graph from a sequence of nodes.
  Adjacent nodes are linked. A sequence of nodes can be
  terminated with the keyword :stop. A node can occur
  several times. The call
  (undirected-graph :a :b :c :stop :b :d)
  will generate a star topology with :b in the center.
  The returned graph is a map from nodes to neighbours."
  [& nodes]
  (apply 
   merge-with concat {}
   (for [section (chop-using (partial = :stop) nodes)]
     (case (count section)
	   0 {}
	   1 {(first section) []}
	   (apply
	    merge-with concat
	    (for [[u v] (partition 2 1 section)]
	      {u [v], v [u]}))))))

(defn fully-connected-graph [& nodes]
  (apply merge-with concat
	 (concat
	  (for [u nodes] {u []})
	  (for [u nodes, v nodes :when (not= u v)] {u [v]}))))

(defn node-distances
  "Returns a map from all nodes to their distance from the root.
  edges should, given a node, return its adjacent nodes.
  Unreachable nodes have undefined distances to the root and are 
  not returned in the map."
  [root edges]
  (loop [queue [[root 0]]
	 result {}
	 seen? #{root}]
    (if (seq queue)
      (let [[node dist] (first queue)
	    unvisited-neighbours (filter (comp not seen?) (edges node))
	    seen? (into seen? unvisited-neighbours)
	    queue (concat (rest queue) 
			  (for [neigh unvisited-neighbours] [neigh (inc dist)]))
	    result (assoc result node dist)]
	(recur queue result seen?))
      result)))

(defn graph-distance-fn [edges]
  (let [cached-node-dist (memoize node-distances)]
    (fn [u v] ((cached-node-dist u edges) v))))

