(ns alijn.utils
  (:use clj-todo))

(defn map-on-values
  "Applies f to the values in the map m."
  [f m] (apply merge (map (fn [[k v]] {k (f v)}) m)))

(todo
 "This function could be rewritten as a simple usage
of partition-by."

(defn chop-using 
  "Chops up a coll using pred. Every time pred is true, a new sequence is started."
  [pred coll]
  (rest
   (map 
    first 
    (take-while
     (fn [[taken coll]] (or (seq taken) (seq coll)))
     (iterate
      (fn [[_ coll]]
	(let [not-pred (comp not pred)
	      taken (take-while not-pred coll)
	      remaining (->> coll (drop-while not-pred) (drop-while pred))]
	  [taken remaining]))
      [[] coll])))))
  
)

(defn chop-using-2 [pred coll]
  (->> (partition-by pred coll)
       (filter (comp not pred first))))

(defn same-graph? [graph-1 graph-2]
  "Only works if nodes are exactly the same and graphs are edge maps."
  (= (map-on-values set graph-1)
     (map-on-values set graph-2)))

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

(defn partition-using-sizes 
  "Partitions coll using the sizes from sizes."
  [sizes coll]
  (do
    (assert (= (count coll) (reduce + sizes)))
    (cond
     (seq sizes) 
      (cons 
       (take (first sizes) coll) 
       (partition-using-sizes (rest sizes) (drop (first sizes) coll)))
     :else nil)))

(defn flatten-groups
  "Flattens the groups into one long seq. Returns the flatted sequence
along with a function to unflatten a seq of same size as the flattened
back to the original structure."
  [groups]
  (let [flattened-groups (apply concat groups)
	sizes (map count groups)
	unflattener (partial partition-using-sizes sizes)]
    [flattened-groups unflattener]))

(defn same-elements? [coll-1 coll-2]
  (= (group-by identity coll-1)
     (group-by identity coll-2)))

(defn remove-first 
  "Removes first element which satisfies pred."
  [pred coll]
  (concat (take-while (comp not pred) coll)
	  (rest (drop-while (comp not pred) coll))))

(defn matching-elements?
  "Checks if every element from coll-1 matches an element from
  coll-2. If (matches? a b) and (matches? b c) then (matches? a c).
  A complete pairing from coll-1 to coll-2 must be possible."
  [matches? coll-1 coll-2]
  (loop [coll-1 coll-1
	 coll-2 coll-2]
    (if (not= (count coll-1) (count coll-2)) 
      false
      (if (= 0 (count coll-1))
	true
	(recur 
	 (rest coll-1)
	 (remove-first (partial matches? (first coll-1)) coll-2))))))

(todo
 "Uses [m & ms] along with (cons m ms) to gurantee at least one element.
 Is there a nicer way?"

(defn same-keys? [m & ms] 
  (->> (cons m ms) (map keys) (map set) (apply =)))

)

(defn maps-to-vectors [m & ms] 
  (let [ms (cons m ms)]
    (if (apply same-keys? ms)
      (let [ks (keys m)
	    vs (map #(map % ks) ms)
	    f (fn [v] (zipmap ks v))]
	(cons f vs))
      (throw (new IllegalArgumentException "Maps must have same keys.")))))