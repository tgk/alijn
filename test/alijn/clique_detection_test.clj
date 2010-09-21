(ns alijn.clique-detection-test
  (:use [alijn.clique-detection] :reload-all)
  (:use [clojure.test]
	[alijn utils combinatorics graph]
	clojure.pprint)
  (:import [javax.vecmath Point3d]))

;;;; Correspondance graphs
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

(deftest test-correspondance-graph-from-colored-graphs
  (let [a {:elm :a, :color :red}
	b {:elm :b, :color :red}
	c {:elm :c, :color :black}
	x {:elm :x, :color :red}
	y {:elm :y, :color :black}
	z {:elm :z, :color :red}]
    (is (same-graph?
	 (undirected-graph [a x] [b z] [c y] [a x] :stop [a z] [b x])
	 (correspondance-graph-from-colored-graphs
	  (undirected-graph b a c)
	  (undirected-graph y x z))))))

(defn fully-connected-corresponsdance-graph
  [nodes-1 nodes-2]
  (apply merge-with concat
	 (for [a nodes-1, b nodes-1, u nodes-2, v nodes-2 
	       :when (and (not= a b) (not= u v))]
	   {[a u] [[b v]]})))

(deftest test-fully-connected-correspondance-graph
  (is (same-graph?
       (undirected-graph [:a :u] [:b :v] :stop
			 [:a :v] [:b :u])
       (fully-connected-corresponsdance-graph [:a :b] [:u :v])))
  (is (same-graph?
       (undirected-graph [:a :u] [:b :v] :stop
			 [:a :u] [:c :v] :stop
			 [:a :v] [:b :u] :stop
			 [:a :v] [:c :u] :stop
			 [:b :u] [:c :v] :stop
			 [:b :v] [:c :u])
       (fully-connected-corresponsdance-graph [:a :b :c] [:u :v]))))

(deftest test-correspondance-graph-from-points
  (let [a (Point3d.  0  0  0)
	b (Point3d.  3  0  0)
	c (Point3d.  6  0  0)
	u (Point3d. 11  0  0)
	v (Point3d. 14  0  0)
	w (Point3d. 16  0  0)
	points-1 [a b c]
	points-2 [u v w]]
    (is (same-graph?
	 (undirected-graph [a u] :stop [b u] :stop [c u] :stop
			   [a v] :stop [b v] :stop [c v] :stop
			   [a w] :stop [b w] :stop [c w] :stop
			   [a u] [b v] [c u] :stop
			   [a v] [b u] [c v] :stop)
	 (correspondance-graph-from-points 0 points-1 points-2)))
    (is (same-graph?
	 (undirected-graph [a u] :stop [b u] :stop [c u] :stop
			   [a v] :stop [b v] :stop [c v] :stop
			   [a w] :stop [b w] :stop [c w] :stop
			   [a u] [b v] [c u] :stop
			   [a v] [b u] [c v] :stop)
	 (correspondance-graph-from-points 0.5 points-1 points-2)))
    (is (same-graph?
	 (undirected-graph [a u] :stop [b u] :stop [c u] :stop
			   [a v] :stop [b v] :stop [c v] :stop
			   [a w] :stop [b w] :stop [c w] :stop
			   [a u] [b v] :stop [a u] [c w] :stop
			   [a v] [b u] :stop [a v] [b w] :stop
			   [a w] [b v] :stop [a w] [c u] :stop
			   [b u] [c v] :stop
			   [b v] [c u] :stop [b v] [c w] :stop
			   [b w] [c v])
	 (correspondance-graph-from-points 1 points-1 points-2)))
    (is (same-graph?
	 (undirected-graph [a u] :stop [b u] :stop [c u] :stop
			   [a v] :stop [b v] :stop [c v] :stop
			   [a w] :stop [b w] :stop [c w] :stop
			   [a u] [b v] :stop [a u] [c w] :stop
			   [a v] [b u] :stop [a v] [b w] :stop
			   [a w] [b v] :stop [a w] [c u] :stop
			   [b u] [c v] :stop
			   [b v] [c u] :stop [b v] [c w] :stop
			   [b w] [c v])
	 (correspondance-graph-from-points 1.5 points-1 points-2)))
    (is (same-graph?
	 (undirected-graph [a u] :stop [b u] :stop [c u] :stop
			   [a v] :stop [b v] :stop [c v] :stop
			   [a w] :stop [b w] :stop [c w] :stop
			   [a u] [b v] :stop [a u] [b w] :stop [a u] [c w] :stop
			   [a v] [b u] :stop [a v] [b w] :stop
			   [a w] [b u] :stop [a w] [b v] :stop [a w] [c u] :stop
			   [b u] [c v] :stop [b u] [c w] :stop
			   [b v] [c u] :stop [b v] [c w] :stop
			   [b w] [c u] :stop [b w] [c v])
	 (correspondance-graph-from-points 2 points-1 points-2)))
    (is (same-graph?
	 (undirected-graph [a u] :stop [b u] :stop [c u] :stop
			   [a v] :stop [b v] :stop [c v] :stop
			   [a w] :stop [b w] :stop [c w] :stop
			   [a u] [b v] :stop [a u] [b w] :stop 
			   [a u] [c v] :stop [a u] [c w] :stop
			   [a v] [b u] :stop [a v] [b w] :stop [a v] [c u] :stop
			   [a w] [b u] :stop [a w] [b v] :stop [a w] [c u] :stop
			   [b u] [c v] :stop [b u] [c w] :stop
			   [b v] [c u] :stop [b v] [c w] :stop
			   [b w] [c u] :stop [b w] [c v])
	 (correspondance-graph-from-points 3 points-1 points-2)))
    (is (same-graph?
	 (fully-connected-corresponsdance-graph [a b c] [u v w])
	 (correspondance-graph-from-points 4 points-1 points-2)))
    (is (same-graph?
	 (fully-connected-corresponsdance-graph [a b c] [u v w])
	 (correspondance-graph-from-points 5 points-1 points-2)))
    (is (same-graph?
	 (fully-connected-corresponsdance-graph [a b c] [u v w])
	 (correspondance-graph-from-points 6 points-1 points-2)))
    (is (same-graph?
	 (fully-connected-corresponsdance-graph [a b c] [u v w])
	 (correspondance-graph-from-points 42 points-1 points-2)))))


;;;;;;;;; Pairing ;;;;;;;;;;;;;

(defn- same-pairing?
  [pairing-1 pairing-2]
  (= (apply zipmap pairing-1)
     (apply zipmap pairing-2)))

(deftest test-same-pairing?
  (is      (same-pairing? [[:a] [:x]] [[:a] [:x]]))
  (is (not (same-pairing? [[:a] [:x]] [[:a] [:y]])))
  (is      (same-pairing? [[:a :b] [:x :y]] [[:a :b] [:x :y]]))
  (is      (same-pairing? [[:a :b] [:x :y]] [[:b :a] [:y :x]]))
  (is (not (same-pairing? [[:a :b] [:x :y]] [[:b :a] [:x :y]])))
  (is      (same-pairing? [[:a :b :c] [:x :y :z]] [[:a :c :b] [:x :z :y]]))
  (is (not (same-pairing? [[:a :b :c] [:x :y :z]] [[:a :b :c] [:x :z :y]])))
  (is      (same-pairing? [[] []] [[] []]))
  (is (not (same-pairing? [[:a] [:b]] [[] []])))
  (is (not (same-pairing? [[] []] [[:a] [:b]])))
  (is      (same-pairing? [[:a] [:a]] [[:a] [:a]])))

(defn- same-pairings?
  [pairings-1 pairings-2]
  (and 
   (= (count pairings-1) (count pairings-2))
   (every? 
    (fn [pairing-1] 
      (some (partial same-pairing? pairing-1) pairings-2))
    pairings-1)))

(deftest test-same-pairings?
  (is      (same-pairings? [[[] []]]
			   [[[] []]]))
  (is      (same-pairings? [[[] []] [[] []]]
			   [[[] []] [[] []]]))
  (is      (same-pairings? [[[:a :b] [:x :y]] [[:a :b] [:y :x]]]
			   [[[:a :b] [:y :x]] [[:a :b] [:x :y]]]))
  (is (not (same-pairings? [[[:a :b] [:x :y]] [[:a :b] [:y :x]]]
			   [[[:a :b] [:y :x]] [[:a :b] [:x :z]]])))
  (is (not (same-pairings? [[[:a :b] [:x :y]] 
			    [[:a :b] [:y :x]] 
			    [[:a :b] [:x :z]]]
			   [[[:a :b] [:y :x]] 
			    [[:a :b] [:x :y]]])))
  (is      (same-pairings? [[[:a :b] [:x :y]] 
			    [[:a :b] [:z :x]]]
			   [[[:a :b] [:z :x]]
			    [[:b :a] [:y :x]]]))
  (is      (same-pairings? [[[:a :b] [:x :y]] 
			    [[:a :b] [:z :x]]]
			   [[[:a :b] [:z :x]]
			    [[:b :a] [:y :x]]])))


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

(deftest test-same-graph-on-points-nodes
  (is (same-graph?
       (undirected-graph (Point3d. 0 0 0))
       (undirected-graph (Point3d. 0 0 0))))
  (is (same-graph?
      (undirected-graph (Point3d. 0 0 0) (Point3d. 1 2 3))
      (undirected-graph (Point3d. 0 0 0) (Point3d. 1 2 3))))
  (is (same-graph?
      (undirected-graph (Point3d. 0 0 0) (Point3d. 1 2 3))
      (undirected-graph (Point3d. 1 2 3) (Point3d. 0 0 0))))
  (is (same-graph?
       (undirected-graph (Point3d. 0 0 0) 
			 (Point3d. 1 2 3)
			 (Point3d. 4 5 6)
			 (Point3d. 0 0 0))
       {(Point3d. 0 0 0) [(Point3d. 1 2 3) (Point3d. 4 5 6)]
	(Point3d. 1 2 3) [(Point3d. 0 0 0) (Point3d. 4 5 6)]
	(Point3d. 4 5 6) [(Point3d. 0 0 0) (Point3d. 1 2 3)]}))
  (is (not (same-graph?
	    (undirected-graph (Point3d. 0 0 0) 
			      (Point3d. 1 2 3)
			      (Point3d. 4 5 6))
	    {(Point3d. 0 0 0) [(Point3d. 1 2 3) (Point3d. 4 5 6)]
	     (Point3d. 1 2 3) [(Point3d. 0 0 0) (Point3d. 4 5 6)]
	     (Point3d. 4 5 6) [(Point3d. 0 0 0) (Point3d. 1 2 3)]}))))

(deftest test-possible-pairings-on-correspondance-graphs-from-points
  (let [a (Point3d.  0  0  0) 
	b (Point3d.  3  0  0)
	c (Point3d. 13  2  0)
	d (Point3d. 15  2  0)
	e (Point3d. 16  2  0)
	points-1 [a b]
	points-2 [c d e]]
    (is (same-pairings?
	 [[[a] [d]]
	  [[b] [d]]
	  [[a b] [c e]]
	  [[a b] [e c]]]
	 (possible-pairings 
	  (correspondance-graph-from-points 0 points-1 points-2))))
    (is (same-pairings?
	 [[[a b] [c d]]
	  [[a b] [d c]]
	  [[a b] [c e]]
	  [[a b] [e c]]]
	 (possible-pairings
	  (correspondance-graph-from-points 1 points-1 points-2))))
    (is (same-pairings?
	 [[[a b] [c d]]
	  [[a b] [d c]]
	  [[a b] [c e]]
	  [[a b] [e c]]
	  [[a b] [d e]]
	  [[a b] [e d]]]
	 (possible-pairings
	  (correspondance-graph-from-points 2 points-1 points-2))))
    (is (same-pairings?
	 [[[a b] [c d]]
	  [[a b] [d c]]
	  [[a b] [c e]]
	  [[a b] [e c]]
	  [[a b] [d e]]
	  [[a b] [e d]]]
	 (possible-pairings
	  (correspondance-graph-from-points 3 points-1 points-2))))))

(deftest test-connect-graph
  (is (same-graph?
       {1 [2 3], 2 [3], 3 []}
       (connect-graph [1 2 3] <)))
  (is (same-graph?
       (fully-connected-graph :a :b :c :d)
       (connect-graph [:a :b :c :d] (fn [u v] (not= u v))))))

(deftest test-true-pred
  (is (true-pred))
  (is (true-pred :foo))
  (is (true-pred :foo :bar))
  (is (true-pred false))
  (is (true-pred nil)))

(deftest test-same-color?
  (is (same-color? {:color :red} {:color :red}))
  (is (not (same-color? {:color :red} {:color :black})))
  (is (same-color? {:color :black} {:color :black})))

(deftest test-not-same-underlying-nodes?
  (is (not-same-underlying-node? [:a :x] [:b :y]))
  (is (not (not-same-underlying-node? [:a :x] [:a :y])))
  (is (not (not-same-underlying-node? [:a :x] [:b :x])))
  (is (not (not-same-underlying-node? [:a :x] [:a :x]))))

(deftest test-same-distance?
  (let [pred (same-distance? - -)]
    (is (pred [0 5] [1 6]))
    (is (not (pred [1 5] [1 6])))
    (is (not (pred [2 5] [1 6]))))
  (let [abs-diff (fn [u v] (Math/abs (- u v)))
	pred (same-distance? abs-diff abs-diff)]
    (is (pred [0 5] [1 6]))
    (is (not (pred [1 5] [1 6])))
    (is (pred [2 5] [1 6]))))

(deftest test-combine-predicates
  (let [pred (combine-predicates not-same-underlying-node? (same-distance? - -))]
    (is (pred [0 1] [4 5]))
    (is (not (pred [0 1] [0 1])))
    (is (not (pred [4 5] [4 5])))
    (is (not (pred [1 5] [2 7]))))
  (let [abs-diff (fn [u v] (Math/abs (- u v)))
	pred (combine-predicates 
	      not-same-underlying-node? 
	      (same-distance? abs-diff abs-diff))]
    (is (pred [10 20] [5 25]))
    (is (not (pred [4 5] [4 3])))))

(deftest test-wrapped-edge-predicate
  (let [wrapped-pred (wrapped-edge-predicate not-same-underlying-node?)
	a {:elm :a}
	b {:elm :b}
	c {:elm :c}
	d {:elm :d}]
    (is (wrapped-pred [a b] [c d]))
    (is (not (wrapped-pred [a b] [a d])))
    (is (not (wrapped-pred [a b] [c b])))
    (is (not (wrapped-pred [a b] [a b]))))
  (let [wrapped-pred (wrapped-edge-predicate (same-distance? - -))
	a {:elm 0}
	b {:elm 1}
	c {:elm 2}
	d {:elm 3}]
    (is (wrapped-pred [a c] [b d]))
    (is (wrapped-pred [a b] [c d]))
    (is (not (wrapped-pred [a c] [a d])))
    (is (not (wrapped-pred [a c] [a a])))))

; Rewrite so takes 1, 2 or 3 coordinates, defaults to zero
(defn- red [x] {:elm (Point3d. x 0 0) :color :red})
(defn- black [x] {:elm (Point3d. x 0 0) :color :black})

(deftest test-correspondance-graph-from-colored-points
  (let [b1 (black 0), b2 (black 2), r1 (red 4)
	b3 (black 0), b4 (black 1), r2 (red 4)]
    (is (same-graph?
	 (undirected-graph [b1 b3] [r1 r2] :stop [b1 b4] :stop [b2 b3] :stop [b2 b4])
	 (correspondance-graph-from-colored-points 0 [b1 b2 r1] [b3 b4 r2])))
    (is (same-graph?
	 (undirected-graph [r1 r2] [b1 b3] [b2 b4] [r1 r2] [b1 b4] [b2 b3])
	 (correspondance-graph-from-colored-points 1 [b1 b2 r1] [b3 b4 r2])))
    (is (same-graph?
	 (undirected-graph [r1 r2] [b1 b3] [b2 b4] [r1 r2] [b1 b4] [b2 b3] [r1 r2])
	 (correspondance-graph-from-colored-points 2 [b1 b2 r1] [b3 b4 r2])))
        (is (same-graph?
	 (undirected-graph [r1 r2] [b1 b3] [b2 b4] [r1 r2] [b1 b4] [b2 b3] [r1 r2])
	 (correspondance-graph-from-colored-points 3 [b1 b2 r1] [b3 b4 r2])))))

(deftest test-possible-pairings-from-correspondance-graph-from-colored-points
  (is (same-pairings?
       [[[][]]]
       (possible-pairings 
	(correspondance-graph-from-colored-points 42 [(black 0)] [(red 0)]))))
  (let [b1 (black 0), b2 (black 2), r1 (red 4)
	b3 (black 0), b4 (black 1), r2 (red 4)]
    (is (same-pairings?
	 [[[b1 r1] [b3 r2]]
	  [[b1] [b4]]
	  [[b2] [b3]]
	  [[b2] [b4]]]
	 (possible-pairings
	  (correspondance-graph-from-colored-points 0 [b1 b2 r1] [b3 b4 r2]))))
    (is (same-pairings?
	 [[[b1 b2 r1] [b3 b4 r2]]
	  [[b1 r1] [b4 r2]]
	  [[b1 b2] [b4 b3]]]
	 (possible-pairings
	  (correspondance-graph-from-colored-points 1 [b1 b2 r1] [b3 b4 r2]))))
    (is (same-pairings?
	 [[[b1 b2 r1] [b3 b4 r2]]
	  [[b1 b2 r1] [b3 b4 r2]]]
	 (possible-pairings
	  (correspondance-graph-from-colored-points 2 [b1 b2 r1] [b3 b4 r2]))))))
  

;;; Error found using flexs dataset

(deftest test-minimal-example
  (let [a (Point3d. 0 0 0)
	a-0 {:elm a :color 0}
	a-1 {:elm a :color 1}
	colored-points [a-0 a-1]
	corr-graph (correspondance-graph-from-colored-points 
		    0.0 colored-points colored-points)
	oracle-corr-graph (undirected-graph [a-0 a-0] [a-1 a-1])]
    (is (same-graph? oracle-corr-graph corr-graph)))

  (let [a (Point3d. 0 0 0)
	b (Point3d. 1 1 1)
	a-0 {:elm a :color 0}
	a-1 {:elm a :color 1}
	b-0 {:elm b :color 0}
	colored-points [a-0 a-1 b-0]
	corr-graph (correspondance-graph-from-colored-points 
		    0.0 colored-points colored-points)
	oracle-corr-graph (undirected-graph 
			   [a-0 a-0] [a-1 a-1] [b-0 b-0] [a-0 a-0] :stop
			   [a-0 b-0] [b-0 a-0] [a-0 b-0])]
    (is (same-graph? oracle-corr-graph corr-graph))))


; Points are from carboxyptd-a/1cbx_kpl_h.mol2
(comment deftest test-simplified-example-from-flexs
  (let [a (Point3d. -0.2793, -2.9642, -0.0718)
	b (Point3d. -2.3723, -2.9332, -0.1578)
	c (Point3d. 1.3747, -3.3622, -3.0318)
	e (Point3d. -0.4481333333333335, 
		    -1.9382000000000006, 
		    -6.334633333333335)
	a-0 {:elm a :color 0}
	b-0 {:elm b :color 0}
	c-0 {:elm c :color 0}
	e-2 {:elm e :color 2}
	a-3 {:elm a :color 3}
	b-3 {:elm b :color 3}
	c-3 {:elm c :color 3}
	colored-points [a-0 b-0 c-0 e-2 a-3 b-3 c-3]
	corr-graph (correspondance-graph-from-colored-points
		    0.0 colored-points colored-points)
	oracle-corr-graph (undirected-graph 
			   [a-0 a-0] [b-0 b-0] [c-0 c-0] [a-0 a-0] :stop
			   [a-0 b-0] [b-0 a-0] :stop
			   [b-0 c-0] [c-0 a-0] :stop
			   [c-0 a-0] [a-0 c-0] :stop
			   
			   [a-3 a-3] [b-3 b-3] [c-3 c-3] [a-3 a-3] :stop
			   [a-3 b-3] [b-3 a-3] :stop
			   [b-3 c-3] [c-3 a-3] :stop
			   [c-3 a-3] [a-3 c-3] :stop
			   
			   [a-0 a-0] [e-2 e-2] :stop
			   [a-0 b-0] [e-2 e-2] :stop
			   [a-0 c-0] [e-2 e-2] :stop
			   [b-0 a-0] [e-2 e-2] :stop
			   [b-0 b-0] [e-2 e-2] :stop
			   [b-0 c-0] [e-2 e-2] :stop
			   [c-0 a-0] [e-2 e-2] :stop
			   [c-0 b-0] [e-2 e-2] :stop
			   [c-0 c-0] [e-2 e-2] :stop

			   [a-3 a-3] [e-2 e-2] :stop
			   [a-3 b-3] [e-2 e-2] :stop
			   [a-3 c-3] [e-2 e-2] :stop
			   [b-3 a-3] [e-2 e-2] :stop
			   [b-3 b-3] [e-2 e-2] :stop
			   [b-3 c-3] [e-2 e-2] :stop
			   [c-3 a-3] [e-2 e-2] :stop
			   [c-3 b-3] [e-2 e-2] :stop
			   [c-3 c-3] [e-2 e-2] :stop

			   [a-0 a-0] [a-3 a-3] :stop
			   [a-0 b-0] [a-3 b-3] :stop
			   [a-0 c-0] [a-3 c-3] :stop
			   [b-0 a-0] [b-3 a-3] :stop
			   [b-0 b-0] [b-3 b-3] :stop
			   [b-0 c-0] [b-3 c-3] :stop
			   [c-0 a-0] [c-3 a-3] :stop
			   [c-0 b-0] [c-3 b-3] :stop
			   [c-0 c-0] [c-3 c-3] :stop

			   [a-0 b-0] [b-3 a-3] :stop
			   [a-0 c-0] [c-3 a-3] :stop

			   [b-0 a-0] [a-3 b-3] :stop
			   [b-0 c-0] [c-3 b-3] :stop

			   [c-0 a-0] [a-3 c-3] :stop
			   [c-0 b-0] [b-3 c-0] :stop

			   )]
    (is (same-graph? oracle-corr-graph corr-graph))))

(comment deftest test-example-from-flexs
  (let [a (Point3d. -0.2793, -2.9642, -0.0718)
	b (Point3d. -2.3723, -2.9332, -0.1578)
	c (Point3d. 1.3747, -3.3622, -3.0318)
	d (Point3d. 1.7407, -1.3452, -2.0438)
	e (Point3d. -0.4481333333333335, 
		    -1.9382000000000006, 
		    -6.334633333333335)
	a-0 {:elm a :color 0}
	b-0 {:elm b :color 0}
	c-0 {:elm c :color 0}
	d-0 {:elm d :color 0}
	e-2 {:elm e :color 2}
	a-3 {:elm a :color 3}
	b-3 {:elm b :color 3}
	c-3 {:elm c :color 3}
	d-3 {:elm d :color 3}
	colored-points [a-0 b-0 c-0 d-0 e-2 a-3 b-3 c-3 d-3]
	corr-graph (correspondance-graph-from-colored-points
		    0.0 colored-points colored-points)
	oracle-corr-graph (undirected-graph )]
    (is (same-graph? oracle-corr-graph corr-graph))))