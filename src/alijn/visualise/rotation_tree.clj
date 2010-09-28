(ns alijn.visualise.rotation-tree
  (:use [penumbra opengl]
	[penumbra.opengl.core :only [*view*]]
	[clojure.contrib pprint]
	[alijn io math rotation-tree])
  (:require [penumbra.app :as app]
	    [penumbra.text :as text])
  (:import [org.openscience.cdk Molecule Atom Bond]
	   [javax.vecmath Point3d]
	   [java.awt Color]
	   [org.newdawn.slick.opengl TextureImpl]))

;;; Molecules drawing

(defn- bond-atoms [bond] (seq (.atoms bond)))

(defn midway [u v] (doto (Point3d.) (.interpolate u v 0.5)))

(def atom-color
     {"C" [0.5 0.5 0.5]
      "H" [1.0 1.0 1.0]
      "N" [0.0 0.0 1.0]
      "O" [1.0 0.0 0.0]
      "P" [1.0 0.5 0.0]})
(def default-color [0.7 0.5 0.3])

(defn draw-atom [atm p middle]
  (let [t (.getSymbol atm)
	c (get atom-color t default-color)]
  (draw-lines
   (apply color c)
   (vertex (.x p) (.y p) (.z p))
   (vertex (.x middle) (.y middle) (.z middle)))))

(defn draw-bond [bond]
  (let [[a1 a2] (bond-atoms bond)
	[p1 p2] [(.getPoint3d a1) (.getPoint3d a2)]
	middle (midway p1 p2)]
    (draw-atom a1 p1 middle)
    (draw-atom a2 p2 middle)))

(defn draw-molecule [molecule]
  (let [bonds (.bonds molecule)
	bonds-atoms (map bond-atoms bonds)
	point-pairs (map (partial map (memfn getPoint3d)) bonds-atoms)]
    (line-width 4)
    (doseq [bond bonds] (draw-bond bond))))

;;; Controllers

(defn init [state]
  (app/vsync! true)
  (app/title! "alijn")
  (enable :depth-test)
  (enable :alpha-test)
  (enable :blend)
  (blend-func :src-alpha :one-minus-src-alpha)
  state)

(defn avg [numbers] (/ (reduce + numbers) (count numbers)))
(defn calculate-bounding-sphere [molecule]
  (let [points (for [atm (.atoms molecule)] (.getPoint3d atm))
	center-point (vec-center points)
	distances (for [point points] (.distance center-point point))
	diameter (apply max distances)]
  [center-point diameter]))

(defn reshape [[x y width height] state]
  (let [[center diam] (calculate-bounding-sphere (:molecule state))
	diam (* 3 diam)
	left   (- (.x center) diam)
	right  (+ (.x center) diam)
	bottom (- (.y center) diam)
	top    (+ (.y center) diam)
	aspect (/ width height)
	[left right] (if (< aspect 1.0) 
		       [(* aspect left) (* aspect right)] [left right])
	[bottom top] (if (> aspect 1.0)
		       [(/ bottom aspect) (/ top aspect)] [bottom top])
	z-near (* -1 diam)
	z-far  diam]
    (gl-matrix-mode :projection)
    (gl-load-identity-matrix)
    (ortho-view left right bottom top z-near z-far)
    (gl-matrix-mode :modelview)
    (gl-load-identity-matrix))
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

(defn update-config [idx delta state]
  (let [configuration (:configuration state)]
    (if (< idx (count configuration))
      (assoc state :configuration 
	     (assoc configuration idx (+ (configuration idx) delta)))
      state)))

(defn update-molecule [state]
  (assoc state :molecule 
	 (molecule-configuration 
	  (:rotation-tree state) 
	  (:configuration state))))

(def up-keys   ["q" "w" "e" "r" "t" "y" "u" "i" "o"])
(def down-keys ["a" "s" "d" "f" "g" "h" "j" "k" "l"])
(def idxs (merge (zipmap up-keys (iterate inc 0)) (zipmap down-keys (iterate inc 0))))
(def deltas (merge (zipmap up-keys (repeat 0.1)) (zipmap down-keys (repeat -0.1))))

(defn key-type [key state]
  (let [idx (get idxs key 0)
	delta (get deltas key 0.0)]
    (update-molecule (update-config idx delta state))))

(defn display [[delta time] state]
  (rotate (:rot-x state) 1 0 0)
  (rotate (:rot-y state) 0 1 0)
  (draw-molecule (:molecule state)))

;;; Interface
(defn molecules-app [molecule]
  (let [rotation-tree (calculate-rotation-tree molecule)
	configuration (vec (repeat (:degrees-of-freedom rotation-tree) 0.0))]
    [{:display display, :reshape reshape, 
      :mouse-drag mouse-drag, 
      :key-type key-type,
      :init init} 
     {:rot-x 0, :rot-y 0,
      :trans-x 0, :trans-y -0.9, :trans-z -30,
      :molecule molecule
      :rotation-tree rotation-tree
      :configuration configuration}]))
  
(defn show-molecules-app [molecule]
  (apply app/start (molecules-app molecule)))

