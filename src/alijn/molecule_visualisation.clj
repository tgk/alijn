(ns alijn.molecule-visualisation
  (:use [penumbra.opengl]
	[clojure.contrib pprint fcase]
	[alijn io])
  (:require [penumbra.app :as app]
	    [penumbra.text :as text])
  (:import [org.openscience.cdk Molecule Atom Bond]
	   [javax.vecmath Point3d]))

;;; Molecules drawing

(def atom-color
     {"C" [0.5 0.5 0.5]
      "H" [1.0 1.0 1.0]
      "N" [0.0 0.0 1.0]
      "O" [1.0 0.0 0.0]})

(defn midway [u v] (doto (Point3d.) (.interpolate u v 0.5)))

(defn- bond-atoms [bond] (seq (.atoms bond)))

(def default-color [0.7 0.5 0.3])
(defn draw-bond [bond]
  (let [[a1 a2] (seq (.atoms bond))
	[p1 p2] [(.getPoint3d a1) (.getPoint3d a2)]
	[t1 t2] [(.getSymbol a1) (.getSymbol a2)]
	c1 (get atom-color t1 default-color) 
	c2 (get atom-color t2 default-color)
	middle (midway p1 p2)]
    (draw-lines
     (apply color c1)
     (vertex (.x p1) (.y p1) (.z p1))
     (vertex (.x middle) (.y middle) (.z middle))
     (apply color c2)
     (vertex (.x p2) (.y p2) (.z p2))
     (vertex (.x middle) (.y middle) (.z middle)))))

(defn draw-molecule [molecule]
  (let [bonds (.bonds molecule)
	bonds-atoms (map bond-atoms bonds)
	point-pairs (map (partial map #(.getPoint3d %)) bonds-atoms)]
    (doseq [bond bonds] (draw-bond bond))))

;;; Feature drawing
; Could change this with standard lib (java.awt.color)
(defn abs [val] (Math/abs (double val)))
(defn in-interval? [[lo hi] val] (and (<= lo val) (< val hi)))
(defn rgb [h s l]
  (let [c       (if (< l 0.5) (* 2 l s) (* (- 2 (* 2 l)) s))
	h-prime (/ h 60)
	x       (* c (- 1 (abs (mod h-prime (- 2 1)))))
	[r1 g1 b1] (fcase in-interval? h-prime
			  [0 1] [c x 0]
			  [1 2] [x c 0]
			  [2 3] [0 c x]
			  [3 4] [0 x c]
			  [4 5] [x 0 c]
			  [5 6] [c 0 x])
	m (- l (/ c 2))
	[r g b] (map (partial + m) [r1 g1 b1])]
  [r g b]))

(defn feature-color-fn [names]
  (let [names (distinct names)
	angle-diff (/ 360 (count names))
	; could be done with simpler range call
	angles (map (partial * angle-diff) (range (count names)))
	hues (zipmap names angles)]
    (fn [name] (rgb (hues name) 0.5 0.5))))

; Cube from penumbra wiki
(defn quad []
  (push-matrix
    (translate -0.5 -0.5 0.5)
    (normal 0 0 -1)
    (vertex 1 1 0)
    (vertex 0 1 0)
    (vertex 0 0 0)
    (vertex 1 0 0)))
 
(defn cube []
  (draw-quads
    (dotimes [_ 4]
      (rotate 90 0 1 0) 
      (quad))
    (rotate 90 1 0 0)
    (quad)
    (rotate 180 1 0 0)
    (quad)))

(defn draw-features [features]
  (let [features (partition 2 features)
	names (map first features)
	feature-color (feature-color-fn names)]
    (doseq [[name pos] features]
      (let [[r g b] (feature-color name)]
	(push-matrix
	 (color r g b 0.5)
	 (translate (.x pos) (.y pos) (.z pos))
	 (cube))))))
    

;;; Controllers

(defn init [state]
  (app/vsync! true)
  state)

(defn reshape [[x y width height] state]
  (comment ortho-view -10 10 -10 10 1.0 100.0)
  (frustum-view 60.0 (/ (double width) height) 1.0 100.0)
  (load-identity)
  state)

(defn mouse-drag [[dx dy] [x y] button state]
  (case button
	:left (assoc state
		:rot-x (+ (:rot-x state) dy)
		:rot-y (+ (:rot-y state) dx))
	:right (assoc state
		 :trans-x (+ (:trans-x state) (/ dx 10))
		 :trans-y (+ (:trans-y state) (/ dy 10)))
	:center (assoc state
		  :trans-z (+ (:trans-z state) (/ dx 10) (/ dy 10)))))

(defn display [[delta time] state]
  (translate (:trans-x state) (:trans-y state) (:trans-z state))
  (rotate (:rot-x state) 1 0 0)
  (rotate (:rot-y state) 0 1 0)
  (doseq [mol (:molecules state)] (draw-molecule mol))
  (draw-features (:features state)))

;;; Interface

(defn show-molecules-app [molecules features]
  (app/start 
   {:display display, :reshape reshape, :mouse-drag mouse-drag, :init init} 
   {:rot-x 0, :rot-y 0,
    :trans-x 0, :trans-y -0.9, :trans-z -30,
    :molecules molecules
    :features features}))

(comment show-molecules-app 
  (take 1 (read-sdf-file "data/debug/carboxy.sdf"))
  ["donor" (Point3d. 0 0 0)
   "acceptor" (Point3d. 1 1 1)
   "donor" (Point3d. 2 2 0)
   "foobar" (Point3d. 0 2 3)
   "bar" (Point3d. 1 2 3)
   "baz" (Point3d. 0 -2 -3)])