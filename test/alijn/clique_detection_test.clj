(ns alijn.clique-detection-test
  (:use [alijn.clique-detection] :reload-all)
  (:use [clojure.test]
	[alijn utils]
	clj-todo))

(deftest test-node-distance

  (is (= {:root 0} (node-distances :root {:root []})))

  (let [graph {:a [:b], :b [:a :c], :c [:b]}]
    (is (= {:a 0, :b 1, :c 2} (node-distances :a graph)))
    (is (= {:a 1, :b 0, :c 1} (node-distances :b graph)))
    (is (= {:a 2, :b 1, :c 0} (node-distances :c graph))))

  (let [graph {:a [:b :c :d], :b [:a], :c [:a], :d [:a]}]
    (is (= {:a 0, :b 1, :c 1, :d 1} (node-distances :a graph)))
    (is (= {:a 1, :b 0, :c 2, :d 2} (node-distances :b graph)))
    (is (= {:a 1, :b 2, :c 0, :d 2} (node-distances :c graph)))
    (is (= {:a 1, :b 2, :c 2, :d 0} (node-distances :d graph))))

  (let [graph {:a [:b], :b [:a :c], :c [:b :d :e], :d [:c], :e [:c]}]
    (is (= {:a 0, :b 1, :c 2, :d 3, :e 3} (node-distances :a graph)))
    (is (= {:a 1, :b 0, :c 1, :d 2, :e 2} (node-distances :b graph)))
    (is (= {:a 2, :b 1, :c 0, :d 1, :e 1} (node-distances :c graph)))
    (is (= {:a 3, :b 2, :c 1, :d 0, :e 2} (node-distances :d graph)))
    (is (= {:a 3, :b 2, :c 1, :d 2, :e 0} (node-distances :e graph))))

  (let [graph {:a [:b], :b [:a], :c [:d :e], :d [:c], :e [:c]}]
    (is (= {:a 0, :b 1}       (node-distances :a graph)))
    (is (= {:a 1, :b 0}       (node-distances :b graph)))
    (is (= {:c 0, :d 1, :e 1} (node-distances :c graph)))
    (is (= {:c 1, :d 0, :e 2} (node-distances :d graph)))
    (is (= {:c 1, :d 2, :e 0} (node-distances :e graph)))))



(deftest test-correspondance-graph-from-graph

  (is (= {[:a :x] []}
	 (correspondance-graph-from-graph {:a []} {:x []})))
  
  (is (= {[:a :x] [[:b :y]] 
	  [:a :y] [[:b :x]]
	  [:b :x] }
	 (correspondance-graph-from-graph
	  {:a [:b], :b [:a]} {:x [:y], :y [:x]})