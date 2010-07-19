(ns alijn.utils
  (:use [clj-todo todo]))

;;; Functions added in 1.2, delete when updating
(todo
"This function actually is in clojure 1.2, so this can be removed when updating."
(defn group-by
  "Groups the seq by the keys generated from group-fn."
  [group-fn seq]
  (apply merge-with concat (map (fn [elm] {(group-fn elm) [elm]}) seq)))
)

(defn rand-nth [coll]
  (nth coll (rand-int (count coll))))

;;; end of 1.2 functionality

;;; Other helpers
(defn map-on-values
  "Applies f to the values in the map m."
  [f m] (apply merge (map (fn [[k v]] {k (f v)}) m)))

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
  
