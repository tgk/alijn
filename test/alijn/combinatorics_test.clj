(ns alijn.combinatorics-test
  (:use [alijn.combinatorics] :reload-all)
  (:use [clojure.test]
	[clj-todo.todo]))

(deftest test-shorter-permutations
  (is (= '(()) (shorter-permutations [1 2] 0)))
  (is (= '((1 2) (2 1)) (shorter-permutations [1 2] 2)))
  (is (= '((1 2) (2 1)) (shorter-permutations [1 2] 3)))
  (is (= '((1 2) (2 1) (1 3) (3 1) (1 4) (4 1) (2 3) (3 2) (2 4) (4 2) (3 4) (4 3))
	 (shorter-permutations [1 2 3 4] 2))))

(deftest test-all-pairs
  (is (= '(([1 4] [2 5]) ([2 4] [1 5]) ([1 4] [3 5]) ([3 4] [1 5]) ([1 4] [6 5]) 
	   ([6 4] [1 5]) ([2 4] [3 5]) ([3 4] [2 5]) ([2 4] [6 5]) ([6 4] [2 5]) 
	   ([3 4] [6 5]) ([6 4] [3 5]))
	 (all-pairs [1 2 3 6] [4 5])))
  (is (= '(([4 1] [5 2]) ([4 2] [5 1]) ([4 1] [5 3]) ([4 3] [5 1]) ([4 1] [5 6])
	   ([4 6] [5 1]) ([4 2] [5 3]) ([4 3] [5 2]) ([4 2] [5 6]) ([4 6] [5 2]) 
	   ([4 3] [5 6]) ([4 6] [5 3]))
	 (all-pairs [4 5] [1 2 3 6])))
  (is (= '(()) (all-pairs [] [1 2 3])))
  (is (= '(()) (all-pairs [1 2 3] []))))

(deftest test-all-grouped-pairs
  (is (= [[[:f1 :f3] [:b1 :b2]] 
	  [[:f1 :f3] [:b1 :b3]] 
	  [[:f2 :f3] [:b1 :b2]] 
	  [[:f2 :f3] [:b1 :b3]]]
	 (grouped-all-pairs [[:f1 :f2] [:b1]] 
			    [[:f3] [:b2 :b3]]))))

(deftest test-leave-one-out
  (is (= [[:a [:b :c :d]]
	  [:b [:a :c :d]]
	  [:c [:a :b :d]]
	  [:d [:a :b :c]]]
	 (leave-one-out [:a :b :c :d])))
  (is (= [[:foo []]] 
	 (leave-one-out [:foo])))
  (is (= [] 
	 (leave-one-out []))))