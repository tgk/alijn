(ns alijn.combinatorics-test
  (:use [alijn.combinatorics] :reload-all)
  (:use [clojure.test]))

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
  (is (= [
	  [["ab" "d"] ["fg" "h"]]
	  [["ab" "e"] ["fg" "h"]]
	  [["ba" "d"] ["fg" "h"]]
	  [["ba" "e"] ["fg" "h"]]
	  [["ac" "d"] ["fg" "h"]]
	  [["ac" "e"] ["fg" "h"]]
	  [["ca" "d"] ["fg" "h"]]
	  [["ca" "e"] ["fg" "h"]]
	  [["bc" "d"] ["fg" "h"]]
	  [["bc" "e"] ["fg" "h"]]
	  [["cb" "d"] ["fg" "h"]]
	  [["cb" "e"] ["fg" "h"]]
	  ]
	 (map 
	  (partial map (partial map (partial apply str)))
	  (all-grouped-pairs ["abc" "de"] ["fg" "h"])))))

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