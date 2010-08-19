(ns alijn.clique-detection-test
  (:use [alijn.clique-detection] :reload-all)
  (:use [clojure.test]
	[alijn utils]
	clj-todo))

(deftest test-node-distance

  (is (= {:root 0} (node-distances :root {:root []})))

  (let [graph (double-linked-graph :a :b :c)]
    (is (= {:a 0, :b 1, :c 2} (node-distances :a graph)))
    (is (= {:a 1, :b 0, :c 1} (node-distances :b graph)))
    (is (= {:a 2, :b 1, :c 0} (node-distances :c graph))))

  (let [graph (double-linked-graph :a :b :stop :a :c :stop :a :d)]
    (is (= {:a 0, :b 1, :c 1, :d 1} (node-distances :a graph)))
    (is (= {:a 1, :b 0, :c 2, :d 2} (node-distances :b graph)))
    (is (= {:a 1, :b 2, :c 0, :d 2} (node-distances :c graph)))
    (is (= {:a 1, :b 2, :c 2, :d 0} (node-distances :d graph))))

  (let [graph (double-linked-graph :a :b :c :d :stop :c :e)]
    (is (= {:a 0, :b 1, :c 2, :d 3, :e 3} (node-distances :a graph)))
    (is (= {:a 1, :b 0, :c 1, :d 2, :e 2} (node-distances :b graph)))
    (is (= {:a 2, :b 1, :c 0, :d 1, :e 1} (node-distances :c graph)))
    (is (= {:a 3, :b 2, :c 1, :d 0, :e 2} (node-distances :d graph)))
    (is (= {:a 3, :b 2, :c 1, :d 2, :e 0} (node-distances :e graph))))

  (let [graph (double-linked-graph :a :b :stop :c :d :stop :c :e)]
    (is (= {:a 0, :b 1}       (node-distances :a graph)))
    (is (= {:a 1, :b 0}       (node-distances :b graph)))
    (is (= {:c 0, :d 1, :e 1} (node-distances :c graph)))
    (is (= {:c 1, :d 0, :e 2} (node-distances :d graph)))
    (is (= {:c 1, :d 2, :e 0} (node-distances :e graph)))))



(deftest test-correspondance-graph-from-graph

  (is (= (double-linked-graph [:a :x])
	 (correspondance-graph-from-graph {:a []} {:x []})))
  
  (is (= (double-linked-graph [:a :x] [:b :y] :stop
			      [:a :y] [:b :x])
	 (correspondance-graph-from-graph
	  (double-linked-graph :a :b)
	  (double-linked-graph :x :y))))

  (let [g1 (double-linked-graph :a :b :stop :a :c :stop :a :d)
	g2 (double-linked-graph :x :y :z)]
    (is (= {[:a :x] '([:d :y] [:c :y] [:b :y]),
	    [:d :z] '([:c :x] [:a :y] [:b :x]),
	    [:d :y] '([:a :z] [:a :x]),
	    [:d :x] '([:c :z] [:a :y] [:b :z]),
	    [:b :z] '([:d :x] [:c :x] [:a :y]),
	    [:c :z] '([:d :x] [:a :y] [:b :x]),
	    [:b :y] '([:a :z] [:a :x]),
	    [:c :y] '([:a :z] [:a :x]),
	    [:b :x] '([:d :z] [:c :z] [:a :y]),
	    [:c :x] '([:d :z] [:a :y] [:b :z]),
	    [:a :z] '([:d :y] [:c :y] [:b :y]),
	    [:a :y] '([:d :z] [:d :x] [:c :z] [:c :x] [:b :z] [:b :x])}
	   (correspondance-graph-from-graph g1 g2)))
    (is (= 
	 (map-on-values 
	  set 
	  {[:a :x] '([:d :y] [:c :y] [:b :y]),
	   [:d :z] '([:c :x] [:a :y] [:b :x]),
	   [:d :y] '([:a :z] [:a :x]),
	   [:d :x] '([:c :z] [:a :y] [:b :z]),
	   [:b :z] '([:d :x] [:c :x] [:a :y]),
	   [:c :z] '([:d :x] [:a :y] [:b :x]),
	   [:b :y] '([:a :z] [:a :x]),
	   [:c :y] '([:a :z] [:a :x]),
	   [:b :x] '([:d :z] [:c :z] [:a :y]),
	   [:c :x] '([:d :z] [:a :y] [:b :z]),
	   [:a :z] '([:d :y] [:c :y] [:b :y]),
	   [:a :y] '([:d :z] [:d :x] [:c :z] [:c :x] [:b :z] [:b :x])})
	 (map-on-values
	  set
	  (correspondance-graph-from-graph g1 g2))))
    (is (= 
	 (map-on-values 
	  set 
	  (double-linked-graph [:a :x] [:b :y] [:a :z] :stop
			       [:a :x] [:c :y] [:a :z] :stop
			       [:a :x] [:d :y] [:a :z] :stop
			       [:a :y] [:b :x] :stop
			       [:a :y] [:c :x] :stop
			       [:a :y] [:d :x] :stop
			       [:a :y] [:b :z] :stop
			       [:a :y] [:c :z] :stop
			       [:a :y] [:d :z] :stop
			       [:b :x] [:c :z] [:d :x] 
			       [:b :z] [:c :x] [:d :z] [:b :x]))
	 (map-on-values
	  set
	  (correspondance-graph-from-graph g1 g2))))))