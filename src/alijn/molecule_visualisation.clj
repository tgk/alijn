(ns alijn.molecule-visualisation
  (:use [penumbra.opengl]
	[clojure.contrib pprint fcase]
	[alijn io])
  (:require [penumbra.app :as app]
	    [penumbra.text :as text])
  (:import [org.openscience.cdk Molecule Atom Bond]
	   [javax.vecmath Point3d]))

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

(defn init [state]
  (app/vsync! true)
  state)

(defn reshape [[x y width height] state]
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
		  :trans-z (+ (:trans-z state) dx dy))))

(defn display [[delta time] state]
  (translate (:trans-x state) (:trans-y state) (:trans-z state))
  (rotate (:rot-x state) 1 0 0)
  (rotate (:rot-y state) 0 1 0)
  (doseq [mol (:molecules state)] (draw-molecule mol)))

(defn show-molecules-app [molecules]
  (app/start 
   {:display display, :reshape reshape, :mouse-drag mouse-drag, :init init} 
   {:rot-x 0, :rot-y 0,
    :trans-x 0, :trans-y -0.9, :trans-z -30,
    :molecules molecules}))

(comment show-molecules-app (take 1 (read-sdf-file "data/debug/carboxy.sdf")))