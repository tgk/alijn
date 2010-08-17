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