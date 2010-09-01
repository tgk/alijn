(ns alijn.graph-test
  (:use [alijn.graph] :reload-all)
  (:use [clojure.test]))

(deftest test-undirected-graph
  (is (= {} (undirected-graph)))
  (is (= {:a []} (undirected-graph :a)))
  (is (= {:a [:b], :b [:a :c], :c [:b]} (undirected-graph :a :b :c)))
  (is (= {:a [], :b []} (undirected-graph :a :stop :b)))
  (is (= {:a [:b], :b [:a], :c []} (undirected-graph :a :b :stop :stop :c)))
  (is (= {:a [:b], :b [:a :c], :c [:b :d :e], :d [:c], :e [:c]}
	 (undirected-graph :a :b :c :d :stop :c :e)))
  (is (= {:a [:b], :b [:a], :c [:d :e], :d [:c], :e [:c]}
	 (undirected-graph :a :b :stop :c :d :stop :c :e)))
  (is (= {:a [:b :d], :b [:a :c], :c [:b :d], :d [:c :a]} 
	 (undirected-graph :a :b :c :d :a))))

(deftest test-fully-connected-graph
  (is (same-graph?
       (fully-connected-graph :a :b)
       (undirected-graph :a :b :stop :b :a)))
  (is (same-graph?
       (fully-connected-graph :a)
       (undirected-graph :a)))
  (is (same-graph?
       (fully-connected-graph :a :b :c)
       (undirected-graph :a :b :c :a :stop)))
  (is (same-graph?
       (fully-connected-graph :a :b :c :d)
       (undirected-graph :a :b :c :d :a :stop
			 :a :c :stop
			 :b :d :stop))))
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

(deftest test-graph-distance-fn
  (let [g (undirected-graph :a :b :c :d :stop :c :e)
	d (graph-distance-fn g)]
    (is (= 0 (d :a :a)))
    (is (= 1 (d :a :b)))
    (is (= 2 (d :a :c)))
    (is (= 3 (d :a :d)))
    (is (= 3 (d :a :e)))

    (is (= 0 (d :a :a)))
    (is (= 1 (d :b :a)))
    (is (= 2 (d :c :a)))
    (is (= 3 (d :d :a)))
    (is (= 3 (d :e :a)))

    (is (= 2 (d :c :a)))
    (is (= 1 (d :c :b)))
    (is (= 0 (d :c :c)))
    (is (= 1 (d :c :d)))
    (is (= 1 (d :c :e)))))
