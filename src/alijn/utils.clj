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