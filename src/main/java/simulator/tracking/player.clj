(ns simulator.tracking.player
  (:import (java.io BufferedWriter FileWriter))
  (:import (java.awt Color Graphics2D Dimension GridBagLayout Insets RenderingHints))
  (:import (java.awt.image BufferedImage))
  (:import (javax.swing JPanel JFrame JButton JTextField JTextArea JLabel JScrollPane))
  (:use [clojure.contrib.math :as math])
  (:use [simulator.types.results :only (get-true-log get-true-events get-strat-log get-strat-events)])
  (:use [simulator.types.generic :only (to-str)])
  (:use [simulator.types.sensors :only (sees)])
  (:use [simulator.tracking.sensors :only (generate-sensors-with-coverage)])
  (:use [simulator.tracking :as tracking :only (run)]))

(def *gridpanel-width* 500)
(def *gridpanel-height* 500)
(def *width* 10)
(def *height* 10)
(def *sensors* nil)
(def *grid-cell-width* 50)
(def *grid-cell-height* 50)
(def *animation-sleep-ms* 500)
(def *running* true)
(def *time* 0)
(def *true-log* nil)
(def *true-events* nil)
(def *strat-log* nil)
(def *strat-events* nil)
(def *grid* (vec (repeat (* *width* *height*) nil)))

(def *steplabel* (JLabel. "Step: "))
(def *true-events-box* (JTextArea. 10 40))
(def *true-log-box* (JTextArea. 10 40))
(def *strat-events-box* (JTextArea. 10 40))
(def *strat-log-box* (JTextArea. 10 40))

(def *mouse-xy* (JLabel.))

(def *param-textfields*
     {:steps (JTextField. 5)
      :numes (JTextField. 5)
      :walk (JTextField. 5)
      :width (JTextField. 5)
      :height (JTextField. 5)
      :strategy (JTextField. 5)
      :sensor-coverage (JTextField. 5)})

(defn get-parameters []
  (let [get-integer (fn [field] (Integer/parseInt (. (field *param-textfields*) getText)))
	get-string (fn [field] (. (field *param-textfields*) getText))]
    [(get-integer :steps) (get-integer :numes) (get-integer :walk)
     (get-integer :width) (get-integer :height) (get-string :strategy)
     (get-integer :sensor-coverage)
     (generate-sensors-with-coverage
       (get-integer :width)
       (get-integer :height)
       (get-integer :sensor-coverage))]))

(defn sensors-see [x y]
  (some #(sees % x y) *sensors*))

(defn get-grid-from-xy [x y]
  [(floor (/ x *grid-cell-width*)) (floor (/ y *grid-cell-height*))])

;; from http://stuartsierra.com/2010/01/08/agents-of-swing

(defmacro with-action [component event & body]
  `(. ~component addActionListener
      (proxy [java.awt.event.ActionListener] []
        (actionPerformed [~event] ~@body))))

(defn grid-at [x y]
  (nth *grid* (+ (* y *width*) x)))

(defn fill-cell [#^Graphics2D g x y c]
  (doto g
    (.setColor c)
    (.fillRect (* x *grid-cell-width*) (* y *grid-cell-height*) *grid-cell-width* *grid-cell-height*)))

(defn draw-move [#^Graphics2D g oldx oldy newx newy c]
  (let [oldpx (+ (* oldx *grid-cell-width*) (/ *grid-cell-width* 2))
	oldpy (+ (* oldy *grid-cell-height*) (/ *grid-cell-height* 2))
	newpx (+ (* newx *grid-cell-width*) (/ *grid-cell-width* 2))
	newpy (+ (* newy *grid-cell-height*) (/ *grid-cell-height* 2))]
    (doto g
      (.setColor c)
      (.drawLine oldpx oldpy newpx newpy)
      (.fillArc (- newpx (/ *grid-cell-width* 10))
		(- newpy (/ *grid-cell-height* 10))
		(/ *grid-cell-width* 5) (/ *grid-cell-height* 5) 0 360))))

(defn draw-grid [g]
  (dorun
   (for [x (range *width*) y (range *height*)]
     (fill-cell g x y (new Color 255 255 255))))
  (dorun
   (for [x (range *width*) y (range *height*)]
     (when (sensors-see x y)
       (fill-cell g x y (new Color 255 180 180 150)))))
  (dorun
   (for [x (range *width*) y (range *height*)]
     (let [event (grid-at x y)]
       (when (not (nil? event))
	 (cond (= (type event) simulator.types.events.EventNew)
	       (fill-cell g x y (new Color 180 180 255 150))
	       (= (type event) simulator.types.events.EventMove)
	       (do
		 (fill-cell g (:x (:oldpos event)) (:y (:oldpos event))
			    (new Color 150 150 150 150))
		 (fill-cell g x y (new Color 100 100 100 150))
		 (draw-move g (:x (:oldpos event)) (:y (:oldpos event)) x y
			    (new Color 0 0 0)))))))))

(defn update-grid []
  (def *grid* (vec (repeat (* *width* *height*) nil)))
  (when *true-events*
    (dorun (for [e (filter #(= (:time %) *time*) *true-events*)]
	     (let [{x :x y :y} (if (= (type e) simulator.types.events.EventMove) (:newpos e) (:pos e))]
	       (def *grid* (assoc *grid* (+ (* y *width*) x) e))))))
  (dorun (for [x (range *width*) y (range *height*)]
           (if (and (not (nil? (grid-at x y))) (< (:time (grid-at x y)) (- *time* 1)))
             (def *grid* (assoc *grid* (+ (* y *width*) x) nil))))))

(defn render [g]
  (let [img (new BufferedImage *gridpanel-width* *gridpanel-height*
		 (. BufferedImage TYPE_INT_ARGB))
        bg (. img (getGraphics))]
    (doto bg
      (.setRenderingHint (. RenderingHints KEY_ANTIALIASING) (. RenderingHints VALUE_ANTIALIAS_ON))
      (.setColor (. Color white))
      (.fillRect 0 0 *gridpanel-width* *gridpanel-height*))
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
	(new Dimension *gridpanel-width* *gridpanel-height*))
       (.addMouseListener
	(proxy [java.awt.event.MouseListener] []
	  (mouseClicked [e])
	  (mouseEntered [e])
	  (mouseExited [e] (. *mouse-xy* setText ""))
	  (mousePressed [e])
	  (mouseReleased [e])))
       (.addMouseMotionListener
	(proxy [java.awt.event.MouseMotionListener] []
	  (mouseMoved [e] (. *mouse-xy* setText
			     (let [[x y] (get-grid-from-xy (. e getX) (. e getY))]
			       (format "Grid %d, %d" x y))))))))

(defn format-event
  [event true-events]
  (if (some #(= event %) true-events)
    (to-str event)
    (str "!" (to-str event))))

(defn update-logs-panel []
  (. *true-events-box* setText (apply str (map to-str (filter #(<= (:time %) *time*) *true-events*))))
  (. *true-log-box* setText (apply str (map to-str (filter #(<= (:time %) *time*) *true-log*))))
  (. *strat-events-box* setText
     (apply str (map (fn [event] (format-event event (filter #(<= (:time %) *time*) *true-events*)))
		     (filter #(<= (:time %) *time*) *strat-events*))))
  (. *strat-log-box* setText (apply str (map to-str (filter #(<= (:time %) *time*) *strat-log*)))))

(defn next-step []
  (when (< *time* (Integer/parseInt (. (:steps *param-textfields*) getText)))
    (def *time* (inc *time*)))
  (update-logs-panel)
  (. *gridpanel* (repaint))
  (. *steplabel* (setText (str "Step: " *time*))))

(defn prev-step []
  (when (> *time* 0)
    (def *time* (dec *time*)))
  (update-logs-panel)
  (. *gridpanel* (repaint))
  (. *steplabel* (setText (str "Step: " *time*))))

(defn run-simulation []
  (let [params (get-parameters)
	result (apply tracking/run params)]
    (def *true-log* (get-true-log result))
    (def *true-events* (get-true-events result))
    (def *strat-log* (get-strat-log result))
    (def *strat-events* (get-strat-events result))
    (def *time* 0)
    (def *width* (Integer/parseInt (. (:width *param-textfields*) getText)))
    (def *height* (Integer/parseInt (. (:height *param-textfields*) getText)))
    (def *grid-cell-width* (/ *gridpanel-width* *width*))
    (def *grid-cell-height* (/ *gridpanel-height* *height*))
    (def *sensors* (last params))
    (update-logs-panel)
    (. *gridpanel* (repaint))
    (. *steplabel* (setText (str "Step: " *time*)))))

(def *newbutton*
     (let [b (JButton. "New")]
       (with-action b e (run-simulation))
       b))

(def *prevbutton*
     (let [b (JButton. "Prev")]
       (with-action b e
	 (prev-step))
       b))

(def *nextbutton*
     (let [b (JButton. "Next")]
       (with-action b e
	 (next-step))
       b))

(def *mainpanel*
     (doto (JPanel. (GridBagLayout.))
       (grid-bag-layout
	:fill :BOTH, :insets (Insets. 5 5 5 5)
	:gridx 0, :gridy 0, :gridheight 11
	*gridpanel*

	:gridx 1, :gridy 0, :gridheight 1
	(JLabel. "Steps:")
	:gridx 2, :gridy 0
	(:steps *param-textfields*)

	:gridx 1, :gridy 1
	(JLabel. "NumberEntities:")
	:gridx 2, :gridy 1
	(:numes *param-textfields*)

	:gridx 1, :gridy 2
	(JLabel. "WalkSize:")
	:gridx 2, :gridy 2
	(:walk *param-textfields*)

	:gridx 1, :gridy 3
	(JLabel. "GridWidth:")
	:gridx 2, :gridy 3
	(:width *param-textfields*)

	:gridx 1, :gridy 4
	(JLabel. "GridHeight:")
	:gridx 2, :gridy 4
	(:height *param-textfields*)

	:gridx 1, :gridy 5
	(JLabel. "Strategy:")
	:gridx 2, :gridy 5
	(:strategy *param-textfields*)

	:gridx 1, :gridy 6
	(JLabel. "SensorCoverage:")
	:gridx 2, :gridy 6
	(:sensor-coverage *param-textfields*)

	:gridx 1, :gridy 7
	*newbutton*
	:gridx 2, :gridy 7
	*steplabel*

	:gridx 1, :gridy 8
	*prevbutton*
	:gridx 2, :gridy 8
	*nextbutton*

	:gridx 1, :gridy 9, :gridwidth 2
	*mouse-xy*

	:gridy 10, :gridwidth 2, :gridheight :REMAINDER
	(JPanel.))))

(def *logspanel*
     (doto (JPanel. (GridBagLayout.))
       (grid-bag-layout
	:fill :BOTH, :insets (Insets. 5 5 5 5)
	
	:gridx 0, :gridy 0
	(JLabel. "True events")
	:gridx 1, :gridy 0
	(JLabel. "True log")
	
	:gridx 0, :gridy 1
	(JScrollPane. *true-events-box*)
	:gridx 1, :gridy 1
	(JScrollPane. *true-log-box*)

	:gridx 0, :gridy 2
	(JLabel. "Strategy events")
	:gridx 1, :gridy 2
	(JLabel. "Strategy log")

	:gridx 0, :gridy 3
	(JScrollPane. *strat-events-box*)
	:gridx 1, :gridy 3
	(JScrollPane. *strat-log-box*))))

(defn start-player []
  (doto (JFrame. "Tracking player")
    (.setContentPane *mainpanel*)
    (.setResizable false)
    ;;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.pack)
    (.show))
  (doto (JFrame. "Logs")
    (.setContentPane *logspanel*)
    (.setResizable false)
    (.pack)
    (.show)))

;;; add sensor coverage overlays in visual grid


