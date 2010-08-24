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

(deftest test-undirected-graph
  (comment is (= {} (undirected-graph)))
  (is (= {:a []} (undirected-graph :a)))
  (is (= {:a [:b], :b [:a :c], :c [:b]} (undirected-graph :a :b :c)))
  (is (= {:a [], :b []} (undirected-graph :a :stop :b)))
  (is (= {:a [:b], :b [:a :c], :c [:b :d :e], :d [:c], :e [:c]}
	 (undirected-graph :a :b :c :d :stop :c :e)))
  (is (= {:a [:b], :b [:a], :c [:d :e], :d [:c], :e [:c]}
	 (undirected-graph :a :b :stop :c :d :stop :c :e)))
  (is (= {:a [:b :d], :b [:a :c], :c [:b :d], :d [:c :a]} 
	 (undirected-graph :a :b :c :d :a))))

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

(deftest test-same-elements?
  (is (true?  (same-elements? [] [])))
  (is (false? (same-elements? [] [1])))
  (is (false? (same-elements? [1] [])))
  (is (true?  (same-elements? [1 2 3] [3 2 1])))
  (is (false? (same-elements? [1 2 3] [3 1])))
  (is (true?  (same-elements? [1 2 2 3] [3 2 1 2])))
  (is (true?  (same-elements? #{1 2 3} #{2 3 1})))
  (is (false? (same-elements? #{1 2 3} #{2 3})))
  (is (true?  (same-elements? #{1 2 3} [2 3 1])))
  (is (false? (same-elements? #{1 2 3} [2 1])))
  (is (true?  (same-elements? {1 2, 3 1} {3 1, 1 2})))
  (is (false? (same-elements? {1 2, 3 2} {3 1, 1 2}))))

(deftest test-remove-first
  (is (= [0 2 3 4] (remove-first odd? [0 1 2 3 4])))
  (is (= [1 2 3 4] (remove-first even? [0 1 2 3 4])))
  (is (= [0 2 4 6 8 10 11 12] (remove-first odd? [0 2 4 6 7 8 10 11 12])))
  (is (= [0 1 3 4] (remove-first (partial = 2) [0 1 2 3 4]))))  

(deftest test-matching-elements?
  (is (true?  (matching-elements? = [] [])))
  (is (false? (matching-elements? = [1] [])))
  (is (false? (matching-elements? = [] [1])))
  (is (true?  (matching-elements? = [1] [1])))

  (is (true?  (matching-elements? = [1 2 3] [3 1 2])))
  (is (false? (matching-elements? = [1 2 3 4] [3 1 2])))
  (is (false? (matching-elements? = [1 2 3] [3 1 4 2])))
  (is (false? (matching-elements? = [1 2 3] [3 1 2 2])))
  (is (false? (matching-elements? = [1 2 3 2] [3 1 2])))
  (is (true?  (matching-elements? = [1 2 3 2] [3 1 2 2])))
  (is (false? (matching-elements? = [1 2 3 4] [3 1 2 0])))

  (let [number-is-str? (fn [n s] (= (str n) s))]
    (is (true?  (matching-elements? number-is-str? [1 2 3] ["1" "3" "2"])))
    (is (false? (matching-elements? number-is-str? [1 2 3] ["1" "2" "2"])))
    (is (false? (matching-elements? number-is-str? [1 2 3] ["1" "2" "2" "3"])))
    (is (true?  (matching-elements? number-is-str? [1 2 3 2] ["1" "2" "2" "3"])))
    (is (false? (matching-elements? number-is-str? [1 2 3 2] ["1" "2" "3" "3"]))))

  (let [xor (fn [a b] (or (and a b) (and (not a) (not b))))
	same-parity? (fn [a b] (xor (even? a) (even? b)))]
    (is (true?  (matching-elements? same-parity? [1 2 3 4] [5 6 7 8])))
    (is (false? (matching-elements? same-parity? [1 2 3 5] [5 6 7 8])))
    (is (true?  (matching-elements? same-parity? [1 2 3 4] [4 3 2 3])))))