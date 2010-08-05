(ns simulator.tracking.player
  (:import (java.io BufferedWriter FileWriter))
  (:import (java.awt Color Graphics Dimension))
  (:import (java.awt.image BufferedImage))
  (:import (javax.swing JPanel JFrame)))


(def *width* 10)
(def *height* 10)
(def *grid-cell-size* 50)
(def *animation-sleep-ms* 500)
(def *running* true)
(def *time* 0)
(def *event-log* nil)
(def *grid* (vec (repeat (* *width* *height*) nil)))

(defn grid-at [x y]
  (nth *grid* (+ (* y *width*) x)))

(defn fill-cell [#^Graphics g x y c]
  (doto g
    (.setColor c)
    (.fillRect (* x *grid-cell-size*) (* y *grid-cell-size*) *grid-cell-size* *grid-cell-size*)))

(defn draw-move [#^Graphics g oldx oldy newx newy c]
  (doto g
    (.setColor c)
    (.drawLine (+ (* oldx *grid-cell-size*) (/ *grid-cell-size* 2))
	       (+ (* oldy *grid-cell-size*) (/ *grid-cell-size* 2))
	       (+ (* newx *grid-cell-size*) (/ *grid-cell-size* 2))
	       (+ (* newy *grid-cell-size*) (/ *grid-cell-size* 2)))))

(defn draw-grid [g]
  (dorun
   (for [x (range *width*) y (range *height*)]
     (cond (nil? (grid-at x y))
	   (fill-cell g x y (new Color 255 255 255 100))
	   (= (type (grid-at x y)) simulator.types.events.EventNew)
	   (fill-cell g x y (new Color 255 0 0 100))
	   (= (type (grid-at x y)) simulator.types.events.EventMove)
	   (do
	     (fill-cell g x y (new Color 0 255 0 100))
	     (fill-cell g (:x (:oldpos (grid-at x y))) (:y (:oldpos (grid-at x y)))
			(new Color 0 100 0 100))
	     (draw-move g (:x (:oldpos (grid-at x y))) (:y (:oldpos (grid-at x y))) x y
			(new Color 0 0 0 100)))))))

(defn update-grid []
  (dorun (for [e (filter #(= (:time %) *time*) *event-log*)]
	   (let [{x :x y :y} (if (= (type e) simulator.types.events.EventMove) (:newpos e) (:pos e))]
	     (def *grid* (assoc *grid* (+ (* y *width*) x) e)))))
  (dorun (for [x (range *width*) y (range *height*)]
	   (if (and (not (nil? (grid-at x y))) (< (:time (grid-at x y)) (- *time* 3)))
	     (def *grid* (assoc *grid* (+ (* y *width*) x) nil))))))

(defn render [g]
  (let [img (new BufferedImage
		 (* *grid-cell-size* *width*)
		 (* *grid-cell-size* *height*)
		 (. BufferedImage TYPE_INT_ARGB))
	bg (. img (getGraphics))]
    (doto bg
      (.setColor (. Color white))
      (.fillRect 0 0 (. img (getWidth)) (. img (getHeight))))
    (update-grid)
    (draw-grid bg)
    (. g (drawImage img 0 0 nil))
    (. bg (dispose))))

(comment
(def *panel*
  (doto (proxy [JPanel] []
	  (paint [g] (render g)))
    (.setPreferredSize
     (new Dimension
	  (* *grid-cell-size* *width*)
	  (* *grid-cell-size* *height*)))))

(def *frame*
  (doto (new JFrame) (.add *panel*) .pack .show))

(def animator (agent nil))

(defn animation [_]
  (when *running*
    (send-off *agent* #'animation))
  (. *panel* (repaint))
  (. Thread (sleep *animation-sleep-ms*))
  (def *time* (inc *time*))
  (if (> *time* 50) (def *running* false))
  nil)
)

;;; add sensor coverage overlays in visual grid

(comment
  (send-off animator animation))


