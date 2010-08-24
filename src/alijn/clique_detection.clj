(ns alijn.clique-detection
  (:use [bron-kerbosch]
	[clojure.contrib.combinatorics]))

(defn node-distances
  "Returns a map from all nodes to their distance from the root.
edges should, given a node, return its adjacent nodes.
Unreachable nodes have undefined distances to the root
and are not returned in the map."
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

(defn correspondance-graph-from-graphs
  [edges-1 edges-2]
  (let [nodes-1 (keys edges-1)
	nodes-2 (keys edges-2)
	cached-node-dist (memoize node-distances)
	dist-1 (fn [a b] ((cached-node-dist a edges-1) b))
	dist-2 (fn [u v] ((cached-node-dist u edges-2) v))
	basis (zipmap (for [a nodes-1, u nodes-2] [a u]) (repeat []))]
    (apply merge-with concat basis
	   (for [a nodes-1 
		 b nodes-1
		 u nodes-2
		 v nodes-2
		 :when (and (not= a b) (not= u v)
			    (= (dist-1 a b) (dist-2 u v)))]
	     {[a u] [[b v]]}))))

(defn possible-pairings
  [correspondance-graph]
  (let [nodes (keys correspondance-graph)]
    (for [cliques (maximum-cliques nodes correspondance-graph)]
      [(map first cliques) (map second cliques)])))

(defn possible-pairings-of-multiple-correspondance-graphs
  [& correspondance-graphs]
  (apply cartesian-product (map possible-pairings correspondance-graphs)))