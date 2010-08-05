(ns simulator.tracking.player
  (:import (java.io BufferedWriter FileWriter))
  (:import (java.awt Color Graphics Dimension GridBagLayout Insets))
  (:import (java.awt.image BufferedImage))
  (:import (javax.swing JPanel JFrame JButton JTextField JLabel)))

(def *width* 10)
(def *height* 10)
(def *grid-cell-size* 50)
(def *animation-sleep-ms* 500)
(def *running* true)
(def *time* 0)
(def *event-log* nil)
(def *grid* (vec (repeat (* *width* *height*) nil)))

(def animator (agent nil))

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

;;; from http://stuartsierra.com/2010/01/05/taming-the-gridbaglayout

(defmacro set-grid! [constraints field value]
  `(set! (. ~constraints ~(symbol (name field)))
         ~(if (keyword? value)
            `(. java.awt.GridBagConstraints
                ~(symbol (name value)))
            value)))

;;; from http://stuartsierra.com/2010/01/05/taming-the-gridbaglayout

(defmacro grid-bag-layout [container & body]
  (let [c (gensym "c")
        cntr (gensym "cntr")]
    `(let [~c (new java.awt.GridBagConstraints)
           ~cntr ~container]
       ~@(loop [result '() body body]
           (if (empty? body)
             (reverse result)
             (let [expr (first body)]
               (if (keyword? expr)
                 (recur (cons `(set-grid! ~c ~expr
                                          ~(second body))
                              result)
                        (next (next body)))
                 (recur (cons `(.add ~cntr ~expr ~c)
                              result)
                        (next body)))))))))

(def *gridpanel*
     (doto (proxy [JPanel] []
	     (paint [g] (render g)))
       (.setPreferredSize
	(new Dimension
	     (* *grid-cell-size* *width*)
	     (* *grid-cell-size* *height*)))))

(def *panel*
     (doto (JPanel. (GridBagLayout.))
       (grid-bag-layout
	:fill :BOTH, :insets (Insets. 5 5 5 5)
	:gridx 0, :gridy 0, :gridheight 8
	*gridpanel*

	:gridx 1, :gridy 0, :gridheight 1
	(JLabel. "Steps:")
	:gridx 2, :gridy 0
	(JTextField. 5)

	:gridx 1, :gridy 1
	(JLabel. "NumberEntities:")
	:gridx 2, :gridy 1
	(JTextField. 5)

	:gridx 1, :gridy 2
	(JLabel. "WalkSize:")
	:gridx 2, :gridy 2
	(JTextField. 5)

	:gridx 1, :gridy 3
	(JLabel. "GridWidth:")
	:gridx 2, :gridy 3
	(JTextField. 5)

	:gridx 1, :gridy 4
	(JLabel. "GridHeight:")
	:gridx 2, :gridy 4
	(JTextField. 5)

	:gridx 1, :gridy 5
	(JLabel. "Strategy:")
	:gridx 2, :gridy 5
	(JTextField. 5)

	:gridx 1, :gridy 6
	(JLabel. "SensorCoverage:")
	:gridx 2, :gridy 6
	(JTextField. 5)

	:gridy 7, :gridwidth 2, :gridheight :REMAINDER
	(JPanel.))))

(defn start-player []
  (doto (JFrame. "Tracking player")
    (.setContentPane *panel*)
    ;;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.pack)
    (.show)))

(defn animation [_]
  (when *running*
    (send-off *agent* #'animation))
  (. *panel* (repaint))
  (. Thread (sleep *animation-sleep-ms*))
  (def *time* (inc *time*))
  (if (> *time* 50) (def *running* false))
  nil)

;;; add sensor coverage overlays in visual grid

(comment
  (send-off animator animation))


