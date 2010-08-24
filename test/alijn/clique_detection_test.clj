(ns alijn.clique-detection-test
  (:use [alijn.clique-detection] :reload-all)
  (:use [clojure.test]
	[alijn utils]
	clojure.pprint
	clj-todo))

(deftest test-node-distance

  (is (= {:root 0} (node-distances :root {:root []})))

  (let [graph (undirected-graph :a :b :c)]
    (is (= {:a 0, :b 1, :c 2} (node-distances :a graph)))
    (is (= {:a 1, :b 0, :c 1} (node-distances :b graph)))
    (is (= {:a 2, :b 1, :c 0} (node-distances :c graph))))

  (let [graph (undirected-graph :a :b :stop :a :c :stop :a :d)]
    (is (= {:a 0, :b 1, :c 1, :d 1} (node-distances :a graph)))
    (is (= {:a 1, :b 0, :c 2, :d 2} (node-distances :b graph)))
    (is (= {:a 1, :b 2, :c 0, :d 2} (node-distances :c graph)))
    (is (= {:a 1, :b 2, :c 2, :d 0} (node-distances :d graph))))

  (let [graph (undirected-graph :a :b :c :d :stop :c :e)]
    (is (= {:a 0, :b 1, :c 2, :d 3, :e 3} (node-distances :a graph)))
    (is (= {:a 1, :b 0, :c 1, :d 2, :e 2} (node-distances :b graph)))
    (is (= {:a 2, :b 1, :c 0, :d 1, :e 1} (node-distances :c graph)))
    (is (= {:a 3, :b 2, :c 1, :d 0, :e 2} (node-distances :d graph)))
    (is (= {:a 3, :b 2, :c 1, :d 2, :e 0} (node-distances :e graph))))

  (let [graph (undirected-graph :a :b :stop :c :d :stop :c :e)]
    (is (= {:a 0, :b 1}       (node-distances :a graph)))
    (is (= {:a 1, :b 0}       (node-distances :b graph)))
    (is (= {:c 0, :d 1, :e 1} (node-distances :c graph)))
    (is (= {:c 1, :d 0, :e 2} (node-distances :d graph)))
    (is (= {:c 1, :d 2, :e 0} (node-distances :e graph))))

  (let [graph (undirected-graph :a :b :c :a)]
    (is (= {:a 0, :b 1, :c 1} (node-distances :a graph)))
    (is (= {:a 1, :b 0, :c 1} (node-distances :b graph)))
    (is (= {:a 1, :b 1, :c 0} (node-distances :c graph)))))


(defn same-graph? [graph-1 graph-2]
  "Only works if nodes are exactly the same and graphs are edge maps."
  (= (map-on-values set graph-1)
     (map-on-values set graph-2)))

(deftest test-correspondance-graph-from-graphs

  (is (same-graph?
       (undirected-graph [:a :x])
       (correspondance-graph-from-graphs {:a []} {:x []})))
  
  (is (same-graph?
       (undirected-graph [:a :x] [:b :y] :stop
			 [:a :y] [:b :x])
       (correspondance-graph-from-graphs
	(undirected-graph :a :b)
	(undirected-graph :x :y))))

  (is (same-graph?
       (undirected-graph [:a :x] [:b :y] [:a :z] :stop
			 [:a :x] [:c :y] [:a :z] :stop
			 [:a :x] [:d :y] [:a :z] :stop
			 [:a :y] [:b :x] :stop
			 [:a :y] [:c :x] :stop
			 [:a :y] [:d :x] :stop
			 [:a :y] [:b :z] :stop
			 [:a :y] [:c :z] :stop
			 [:a :y] [:d :z] :stop
			 [:b :x] [:c :z] [:d :x] 
			 [:b :z] [:c :x] [:d :z] [:b :x])
       (correspondance-graph-from-graphs 
	(undirected-graph :a :b :stop :a :c :stop :a :d)
	(undirected-graph :x :y :z))))

  (is (same-graph? 
       (undirected-graph [:a :x] [:b :y] [:c :x] 
			 [:a :y] [:b :x] [:c :y] [:a :x])
       (correspondance-graph-from-graphs
	(undirected-graph :a :b :c :a)
	(undirected-graph :x :y)))))

(deftest test-correspondance-graph-on-triplets
  (is (same-graph?
       (undirected-graph [:a :x] [:b :y] [:a :z] [:b :x] [:a :y] [:b :z] [:a :x] :stop
			 [:c :x] :stop [:c :y] :stop [:c :z])
       (correspondance-graph-from-graphs
	(undirected-graph :a :b :stop :c)
	(undirected-graph :x :y :z :x))))
  (is (same-graph?
       (undirected-graph [:a :x] [:b :y] [:c :z] [:a :x] :stop
			 [:a :y] [:b :x] [:c :z] [:a :y] :stop
			 [:a :z] [:c :x] [:b :z] :stop 
			 [:a :z] [:c :y] [:b :z] :stop)
       (correspondance-graph-from-graphs
	(undirected-graph :a :b :stop :c)
	(undirected-graph :x :y :stop :z)))))


(defn- same-pairing?
  [pairing-1 pairing-2]
  (= (apply zipmap pairing-1)
     (apply zipmap pairing-2)))

(deftest test-same-pairing?
  (is (true?  (same-pairing? [[:a] [:x]] [[:a] [:x]])))
  (is (false? (same-pairing? [[:a] [:x]] [[:a] [:y]])))
  (is (true?  (same-pairing? [[:a :b] [:x :y]] [[:a :b] [:x :y]])))
  (is (true?  (same-pairing? [[:a :b] [:x :y]] [[:b :a] [:y :x]])))
  (is (false? (same-pairing? [[:a :b] [:x :y]] [[:b :a] [:x :y]])))
  (is (true?  (same-pairing? [[:a :b :c] [:x :y :z]] [[:a :c :b] [:x :z :y]])))
  (is (false? (same-pairing? [[:a :b :c] [:x :y :z]] [[:a :b :c] [:x :z :y]])))

  (is (true?  (same-pairing? [[] []] [[] []])))
  (is (false? (same-pairing? [[:a] [:b]] [[] []])))
  (is (false? (same-pairing? [[] []] [[:a] [:b]])))
  (is (true?  (same-pairing? [[:a] [:a]] [[:a] [:a]]))))

(defn- same-pairings?
  [pairings-1 pairings-2]
  (and 
   (= (count pairings-1) (count pairings-2))
   (every? 
    (fn [pairing-1] 
      (some (partial same-pairing? pairing-1) pairings-2))
    pairings-1)))

(deftest test-same-pairings?
  (is (true?  (same-pairings? [[[] []]]
			      [[[] []]])))
  (is (true?  (same-pairings? [[[] []] [[] []]]
			      [[[] []] [[] []]])))
  (is (true?  (same-pairings? [[[:a :b] [:x :y]] [[:a :b] [:y :x]]]
			      [[[:a :b] [:y :x]] [[:a :b] [:x :y]]])))
  (is (false? (same-pairings? [[[:a :b] [:x :y]] [[:a :b] [:y :x]]]
			      [[[:a :b] [:y :x]] [[:a :b] [:x :z]]])))
  (is (false? (same-pairings? [[[:a :b] [:x :y]] 
			       [[:a :b] [:y :x]] 
			       [[:a :b] [:x :z]]]
			      [[[:a :b] [:y :x]] 
			       [[:a :b] [:x :y]]])))
  (is (true?  (same-pairings? [[[:a :b] [:x :y]] 
			       [[:a :b] [:z :x]]]
			      [[[:a :b] [:z :x]]
			       [[:b :a] [:y :x]]])))
  (is (true?  (same-pairings? [[[:a :b] [:x :y]] 
			       [[:a :b] [:z :x]]]
			      [[[:a :b] [:z :x]]
			       [[:b :a] [:y :x]]]))))


(defn- possible-pairings-from-graph-defs [def-1 def-2]
  (possible-pairings
   (correspondance-graph-from-graphs
    (apply undirected-graph def-1) 
    (apply undirected-graph def-2))))

(deftest test-possible-pairings

  (is (same-pairings?
       [[[:a] [:x]]
	[[:b] [:x]]]
       (possible-pairings-from-graph-defs [:a :b] [:x])))
       
  (is (same-pairings? 
       [[[:a :b] [:x :y]]
	[[:a :c] [:x :y]]
	[[:a :d] [:x :y]]
	[[:a :b :c] [:y :x :z]]
	[[:a :b :c] [:y :z :x]]
	[[:a :b :d] [:y :x :z]]
	[[:a :b :d] [:y :z :x]]
	[[:a :c :d] [:y :x :z]]
	[[:a :c :d] [:y :z :x]]
	[[:a :b] [:z :y]]
	[[:a :c] [:z :y]]
	[[:a :d] [:z :y]]]
       (possible-pairings-from-graph-defs [:b :a :d :stop :a :c] [:x :y :z])))

  (is (same-pairings?
       [[[:a :b] [:x :y]]
	[[:a :b] [:y :x]]
	[[:a :c] [:x :y]]
	[[:a :c] [:y :x]]
	[[:b :c] [:x :y]]
	[[:b :c] [:y :x]]]
       (possible-pairings-from-graph-defs [:a :b :c :a] [:x :y]))))

(deftest test-possible-pairings-duplets
  (is (same-pairings?
       [[[:a :b] [:x :y]]
	[[:a :b] [:y :x]]]
       (possible-pairings-from-graph-defs [:a :stop :b] [:x :stop :y])))
  (is (same-pairings?
       [[[:a :b] [:x :y]]
	[[:a :b] [:y :x]]]
       (possible-pairings-from-graph-defs [:a :b] [:x :y])))
  (is (same-pairings?
       [[[:a] [:x]]
	[[:a] [:y]]
	[[:b] [:x]]
	[[:b] [:y]]]
       (possible-pairings-from-graph-defs [:a :stop :b] [:x :y]))))

(deftest test-possible-pairings-triplets
  (is (same-pairings?
       [[[:a :b :c] [:x :y :z]]
	[[:a :b :c] [:x :z :y]]
	[[:a :b :c] [:y :z :x]]
	[[:a :b :c] [:y :x :z]]
	[[:a :b :c] [:z :x :y]]
	[[:a :b :c] [:z :y :x]]]
       (possible-pairings-from-graph-defs [:a :b :c :a] [:x :y :z :x])))

  (is (same-pairings?			
       [[[:a :b] [:x :y]]
	[[:a :b] [:x :z]]
	[[:a :b] [:y :x]]
	[[:a :b] [:y :z]]
	[[:a :b] [:z :x]]
	[[:a :b] [:z :y]]
	[[:b :c] [:x :y]]
	[[:b :c] [:x :z]]
	[[:b :c] [:y :x]]
	[[:b :c] [:y :z]]
	[[:b :c] [:z :x]]
	[[:b :c] [:z :y]]]
       (possible-pairings-from-graph-defs [:a :b :c] [:x :y :z :x])))

  (is (same-pairings?
       [[[:a :b] [:x :y]]
	[[:a :b] [:y :x]]
	[[:a :b] [:z :y]]
	[[:a :b] [:y :z]]
	[[:a :b] [:x :z]]
	[[:a :b] [:z :x]]
	[[:c] [:x]]
	[[:c] [:y]]
	[[:c] [:z]]]
       (possible-pairings-from-graph-defs [:a :b :stop :c] [:x :y :z :x])))

  (is (same-pairings?
       [[[:a :b :c] [:x :y :z]]
	[[:a :b]    [:y :x]]
	[[:a :b]    [:y :z]]
	[[:a :b :c] [:z :y :x]]
	[[:b :c]    [:x :y]]
	[[:b :c]    [:z :y]]]
       (possible-pairings-from-graph-defs [:a :b :c] [:x :y :z])))

  (is (same-pairings?
       [[[:a :b :c] [:x :y :z]]
	[[:a :b :c] [:y :x :z]]
	[[:a :c] [:z :x]]
	[[:a :c] [:z :y]]
	[[:b :c] [:z :x]]
	[[:b :c] [:z :y]]]
       (possible-pairings-from-graph-defs [:a :b :stop :c] [:x :y :stop :z]))))


(defn same-colored-pairing?
  [pairings-1 pairings-2]
  (every? (partial apply same-pairing?) (map vector pairings-1 pairings-2)))

(deftest test-same-colored-pairing?
  (is (same-colored-pairing?
       [[[] []]] [[[] []]]))
  (is (same-colored-pairing?
       [[[] []] [[] []]] [[[] []] [[] []]]))
  (is (not (same-colored-pairing?
       [[[:a] [:b]]] [[[] []]])))
  (is (not (same-colored-pairing?
       [[[] []]] [[[:a] [:b]]])))
  (is (same-colored-pairing? 
    [[[:a :b :c] [:x :y :z]] [[:u :v] [:i :j]]]
    [[[:c :b :a] [:z :y :x]] [[:u :v] [:i :j]]]))
  (is (not 
       (same-colored-pairing?
	[[[:a :b :c] [:x :y :z]] [[:u :v] [:i :j]]]
	[[[:c :b :a] [:x :y :z]] [[:u :v] [:i :j]]]))))

(defn same-multiple-pairings?
  [pairings-1 pairings-2]
  (matching-elements? same-colored-pairing? pairings-1 pairings-2))

(deftest test-same-multiple-pairings?
  (is (same-multiple-pairings?
       [[[[:a :b] [:x :y]] [[:i] [:u]]]
	[[[:a :b] [:y :x]] [[:j] [:u]]]]
       [[[[:b :a] [:x :y]] [[:j] [:u]]]
	[[[:a :b] [:x :y]] [[:i] [:u]]]]))
  (is (not       
       (same-multiple-pairings?
	[[[[:a :b] [:x :y]] [[:i] [:u]]]
	 [[[:a :b] [:y :x]] [[:j] [:u]]]]
	[[[[:a :b] [:x :y]] [[:j] [:u]]]
	 [[[:a :b] [:x :y]] [[:i] [:u]]]])))
  (is (not       
       (same-multiple-pairings?
	[[[[:a :b] [:x :y]] [[:i] [:u]]]
	 [[[:a :b] [:y :x]] [[:j] [:u]]]]
	[[[[:a :b] [:x :y]] [[:j] [:u]]]
	 [[[:a :b] [:y :x]] [[:i] [:u]] [:f :g] [:n :m]]])))
  (is (same-multiple-pairings?
       [[[[:a :b] [:x :y]] [[] []]]
	[[[:a :b] [:y :x]] [[:j] [:u]]]]
       [[[[:b :a] [:x :y]] [[:j] [:u]]]
	[[[:a :b] [:x :y]] [[] []]]])))

(deftest test-possible-pairings-of-multiple-correspondance-graphs
  (is (same-multiple-pairings?
       [[[[:a :b] [:x :y]] [[:i] [:u]]]
	[[[:a :b] [:x :y]] [[:i] [:v]]]
	[[[:a :b] [:x :y]] [[:j] [:u]]]
	[[[:a :b] [:x :y]] [[:j] [:v]]]
	[[[:b :c] [:x :y]] [[:i] [:u]]]
	[[[:b :c] [:x :y]] [[:i] [:v]]]
	[[[:b :c] [:x :y]] [[:j] [:u]]]
	[[[:b :c] [:x :y]] [[:j] [:v]]]
	[[[:a :b] [:y :x]] [[:i] [:u]]]
	[[[:a :b] [:y :x]] [[:i] [:v]]]
	[[[:a :b] [:y :x]] [[:j] [:u]]]
	[[[:a :b] [:y :x]] [[:j] [:v]]]
	[[[:b :c] [:y :x]] [[:i] [:u]]]
	[[[:b :c] [:y :x]] [[:i] [:v]]]
	[[[:b :c] [:y :x]] [[:j] [:u]]]
	[[[:b :c] [:y :x]] [[:j] [:v]]]]
       (possible-pairings-of-multiple-correspondance-graphs
	(correspondance-graph-from-graphs
	 (undirected-graph :a :b :c)
	 (undirected-graph :x :y))
	(correspondance-graph-from-graphs
	 (undirected-graph :i :j)
	 (undirected-graph :u :stop :v)))))
  (is (same-multiple-pairings?
       [[[[:a :b] [:x :y]] [[:i :j] [:u :v]]]
	[[[:a :b] [:x :y]] [[:i :j] [:v :u]]]
	[[[:a :b] [:y :x]] [[:i :j] [:u :v]]]
	[[[:a :b] [:y :x]] [[:i :j] [:v :u]]]
	[[[:b :c] [:x :y]] [[:i :j] [:u :v]]]
	[[[:b :c] [:x :y]] [[:i :j] [:v :u]]]
	[[[:b :c] [:y :x]] [[:i :j] [:u :v]]]
	[[[:b :c] [:y :x]] [[:i :j] [:v :u]]]]
       (possible-pairings-of-multiple-correspondance-graphs
	(correspondance-graph-from-graphs 
	 (undirected-graph :a :b :c)
	 (undirected-graph :x :y))
	(correspondance-graph-from-graphs
	 (undirected-graph :i :j)
	 (undirected-graph :u :v))))))