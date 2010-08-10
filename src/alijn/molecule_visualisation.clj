(ns alijn.molecule-visualisation
  (:use [penumbra opengl]
	[penumbra.opengl.core :only [*view*]]
	[clojure.contrib pprint]
	[alijn io math])
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
      "O" [1.0 0.0 0.0]})
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


;;; Feature drawing
(defn rgb [h s l]
  (let [color (Color/getHSBColor h s l)
	components [(.getRed color) (.getGreen color) (.getBlue color)]]
    (map #(/ % 360) components)))

(defn feature-color-fn [names]
  (let [names (distinct names)
	angle-diff (/ 1 (count names))
	angles (range 0 1 angle-diff)
	hues (zipmap names angles)]
    (fn [name] (rgb (hues name) 0.5 1.0))))

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
  (when (> (count features) 0)
    (let [features (partition 2 features)
	  names (distinct (map first features))
	  feature-color (feature-color-fn names)]
      (doseq [[name offset] (zipmap names (iterate (partial + 30) 0))]
	(let [[r g b] (feature-color name)]
	  (text/write-to-screen name 10 offset)
	  (let [[x-origin y-origin w h] @*view*]
	    (with-projection 
	      (ortho-view x-origin (+ x-origin w) (+ y-origin h) y-origin -1 1)
	      (push-matrix
	       (load-identity)
	       (TextureImpl/bindNone)
	       (color r g b 1.0)
	       (draw-lines
		(vertex 0 (+ 25 offset))
		(vertex 100 (+ 25 offset))))))))
      (doseq [[name pos] features]
	(let [[r g b] (feature-color name)]
	  (push-matrix
	   (color r g b 0.5)
	   (translate (.x pos) (.y pos) (.z pos))
	   (scale 0.5 0.5 0.5)
	   (cube)))))))
    

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
(defn calculate-bounding-sphere [molecules]
  (let [points (apply concat (for [molecule molecules] 
			       (for [atm (.atoms molecule)] (.getPoint3d atm))))
	center-point (vec-center points)
	distances (for [point points] (.distance center-point point))
	diameter (apply max distances)]
  [center-point diameter]))

(defn reshape [[x y width height] state]
  (let [[center diam] (calculate-bounding-sphere (:molecules state))
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

(defn display [[delta time] state]
  (rotate (:rot-x state) 1 0 0)
  (rotate (:rot-y state) 0 1 0)
  (doseq [mol (:molecules state)] (draw-molecule mol))
  (draw-features (:features state)))


;;; Interface
(defn show-molecules-app [molecules features]
  (app/start 
   {:display display, :reshape reshape, 
    :mouse-drag mouse-drag, 
    :init init} 
   {:rot-x 0, :rot-y 0,
    :trans-x 0, :trans-y -0.9, :trans-z -30,
    :molecules molecules
    :features features}))

(comment
  show-molecules-app 
  (take 1 (read-molecules "data/debug/carboxy.sdf"))
  ["donor" (Point3d. 0 0 0)
   "acceptor" (Point3d. 1 1 1)
   "donor" (Point3d. 2 2 0)
   "foobar" (Point3d. 0 2 3)
   "bar" (Point3d. 1 2 3)
   "baz" (Point3d. 0 -2 -3)])