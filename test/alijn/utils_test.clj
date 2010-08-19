(ns alijn.utils-test
  (:use [alijn.utils] :reload-all)
  (:use [clojure.test]))

(deftest test-chop-using
  (is (= '((0) (2) (4) (6) (8))
	 (chop-using odd? (range 10))))
  (is (= '((:a :b :c) (:d :e))
	 (chop-using (partial = :stop) [:a :b :c :stop :d :e])))
  (is (= '((:a :b :c :d :e))
	 (chop-using (partial = :stop) [:a :b :c :d :e]))))

(deftest test-double-linked-graph
  (comment is (= {} (double-linked-graph)))
  (is (= {:a []} (double-linked-graph :a)))
  (is (= {:a [:b], :b [:a :c], :c [:b]} (double-linked-graph :a :b :c)))
  (is (= {:a [], :b []} (double-linked-graph :a :stop :b)))
  (is (= {:a [:b], :b [:a :c], :c [:b :d :e], :d [:c], :e [:c]}
	 (double-linked-graph :a :b :c :d :stop :c :e)))
  (is (= {:a [:b], :b [:a], :c [:d :e], :d [:c], :e [:c]}
	 (double-linked-graph :a :b :stop :c :d :stop :c :e)))
  (is (= {:a [:b :d], :b [:a :c], :c [:b :d], :d [:c :a]} 
	 (double-linked-graph :a :b :c :d :a))))

(deftest test-partition-using-sizes
  (is (= [[:foo] [:bar :baz]] 
	 (partition-using-sizes [1 2] [:foo :bar :baz])))
  (is (= [[:foo :bar] [:baz :foobar]] 
	 (partition-using-sizes [2 2] [:foo :bar :baz :foobar])))
  (is (= [[:foo] [] [:bar] [:baz]] 
	 (partition-using-sizes [1 0 1 1] [:foo :bar :baz])))
  (is (= [[]] 
	 (partition-using-sizes [0] []))))

(deftest test-flatten-groups
  (let [groups [[:foo :bar] [:baz]]
	[flat unflatten-groups] (flatten-groups groups)]
    (is (= [:foo :bar :baz] flat))
    (is (= groups (unflatten-groups flat))))
  (let [groups [[]]
	[flat unflatten-groups] (flatten-groups groups)]
    (is (= [] flat))
    (is (= groups (unflatten-groups flat))))
  (let [groups [[:foo] [] [:bar :baz]]
	[flat unflatten-groups] (flatten-groups groups)]
    (is (= [:foo :bar :baz] flat))
    (is (= groups (unflatten-groups flat)))))
