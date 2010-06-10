(ns alijn.molecule-alignment-test
  (:use [alijn.molecule-alignment] :reload-all)
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


