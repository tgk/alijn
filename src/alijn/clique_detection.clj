(ns alijn.clique-detection)

(defn node-distances 
  "Returns a map from all nodes to their distance from the root.
edges should, given a node, return its adjacent nodes.
Unreachable nodes have undefined distances to the root
and are not returned in the map."
  [root edges]
  (loop [queue [[root 0]]
	 result {}]
    (if (seq queue)
      (let [[node dist] (first queue)
	    queue (concat (rest queue) 
			  (for [neigh (edges node) 
				:when (and
				       (not (contains? result neigh))
				       (not (contains? queue neigh)))] 
			    [neigh (inc dist)]))
	    result (assoc result node dist)]
	(recur queue result))
      result)))

(defn correspondance-graph-from-graph
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