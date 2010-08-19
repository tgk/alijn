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