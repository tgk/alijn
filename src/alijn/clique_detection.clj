(ns alijn.clique-detection
  (:use [bron-kerbosch]
	[clojure.contrib.combinatorics]
	[alijn.math]))

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

(defn within? [threshold n m]
  (cond
   (= n m)   true
   (= nil n) false
   (= nil m) false
   :else     (<= (- threshold) (- m n) threshold)))

(defn correspondance-graph 
  "Create a correspondance graph with the elements from coll-1 and 
  coll-2. dist-1 measures distance in coll-1, dist-2 in coll-2. If a
  and b in coll-1 have a distance that deviates less than threshold 
  from the distance between u and v in coll-2, the nodes [a u] and 
  [b v] are linked."
  [threshold coll-1 coll-2 dist-1 dist-2]
  (let [basis (zipmap (for [a coll-1, u coll-2] [a u]) (repeat []))]
    (apply merge-with concat basis
	   (for [a coll-1 
		 b coll-1
		 u coll-2
		 v coll-2
		 :when (and (not= a b) (not= u v)
			    (within? threshold (dist-1 a b) (dist-2 u v)))]
	     {[a u] [[b v]]}))))

(defn correspondance-graph-from-graphs
  [edges-1 edges-2]
  (let [nodes-1 (keys edges-1)
	nodes-2 (keys edges-2)
	cached-node-dist (memoize node-distances)
	dist-1 (fn [a b] ((cached-node-dist a edges-1) b))
	dist-2 (fn [u v] ((cached-node-dist u edges-2) v))]
    (correspondance-graph 0 nodes-1 nodes-2 dist-1 dist-2)))  

(defn correspondance-graph-from-points
  [threshold points-1 points-2]
  (correspondance-graph threshold points-1 points-2 distance distance))

(defn correspondance-graph-from-colored-points
  [threshold colored-points-1 colored-points-2]
  (map (partial correspondance-graph-from-points threshold)
       colored-points-1 colored-points-2))

(defn possible-pairings
  [correspondance-graph]
  (let [nodes (keys correspondance-graph)]
    (for [cliques (maximum-cliques nodes correspondance-graph)]
      [(map first cliques) (map second cliques)])))

(defn possible-pairings-of-multiple-correspondance-graphs
  [& correspondance-graphs]
  (apply cartesian-product (map possible-pairings correspondance-graphs)))

(defn possible-pairings-of-colored-points
  [threshold colored-points-1 colored-points-2]
  (apply
   possible-pairings-of-multiple-correspondance-graphs
   (correspondance-graph-from-colored-points 
    threshold colored-points-1 colored-points-2)))