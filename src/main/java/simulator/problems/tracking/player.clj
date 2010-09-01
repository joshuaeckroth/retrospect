(ns simulator.problems.tracking.player
  (:import (java.io BufferedWriter FileWriter))
  (:import (java.awt Color Graphics2D Dimension GridBagLayout Insets RenderingHints))
  (:import (java.awt.image BufferedImage))
  (:import (javax.swing JPanel JFrame JButton JTextField JTextArea
			JLabel JScrollPane JSpinner SpinnerNumberModel JComboBox))
  (:require [simulator.problems.tracking eventlog])
  (:import [simulator.problems.tracking.eventlog EventLog])
  (:use [clojure.contrib.math :as math])
  (:use [simulator.problems.tracking.sensors :only (sees)])
  (:use [simulator.problems.tracking.eventlog :only (get-events)])
  (:use [simulator.strategies :only (strategies init-strat-state)])
  (:use [simulator.problems.tracking.core :as tracking :only (run)])
  (:use [simulator.evaluator :only (evaluate)]))

(def *gridpanel-width* 500)
(def *gridpanel-height* 500)
(def *width* 10)
(def *height* 10)
(def *sensors* nil)
(def *grid-cell-width* 50)
(def *grid-cell-height* 50)
(def *time* 0)
(def *abducer-log* nil)
(def *true-events* nil)
(def *strat-log* nil)
(def *strat-events* nil)
(def *grid* (vec (repeat (* *width* *height*) nil)))

(def *steplabel* (JLabel. "Step: "))
(def *resultslabel* (JLabel. "Correct: "))
(def *true-events-box* (JTextArea. 10 40))
(def *abducer-log-box* (JTextArea. 10 40))
(def *strat-events-box* (JTextArea. 10 40))
(def *strat-log-box* (JTextArea. 10 40))

(def *mouse-xy* (JLabel.))

(def *param-spinners*
     {:Steps (JSpinner. (SpinnerNumberModel. 50 1 1000 1))
      :NumberEntities (JSpinner. (SpinnerNumberModel. 1 1 100 1))
      :MaxWalk (JSpinner. (SpinnerNumberModel. 1 1 100 1))
      :ProbNewEntities (JSpinner. (SpinnerNumberModel. 0 0 100 10))
      :SensorReportNoise (JSpinner. (SpinnerNumberModel. 0 0 100 10))
      :BeliefNoise (JSpinner. (SpinnerNumberModel. 0 0 100 10))
      :GridWidth (JSpinner. (SpinnerNumberModel. 10 1 100 1))
      :GridHeight (JSpinner. (SpinnerNumberModel. 10 1 100 1))
      :SensorCoverage (JSpinner. (SpinnerNumberModel. 100 0 100 10))})

(def *params* (apply hash-map (flatten (for [k (keys *param-spinners*)] [k 0]))))

(def *param-other*
     {:Strategy (JComboBox. (to-array strategies))})

(defn get-parameters []
  (apply hash-map (flatten (for [k (keys *param-spinners*)]
			     [k (->> (k *param-spinners*)
				     .getModel .getNumber .intValue)]))))

(defn get-strategy [] (nth strategies (.getSelectedIndex (:Strategy *param-other*))))

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
    (.fillRect (* x *grid-cell-width*) (* y *grid-cell-height*)
	       *grid-cell-width* *grid-cell-height*)))

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
	 (cond (= (type event) simulator.problems.tracking.events.EventNew)
	       (fill-cell g x y (new Color 180 180 255 150))
	       (= (type event) simulator.problems.tracking.events.EventMove)
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
	     (let [{x :x y :y}
		   (if (= (type e) simulator.problems.tracking.events.EventMove)
		     (:newpos e) (:pos e))]
	       (def *grid* (assoc *grid* (+ (* y *width*) x) e))))))
  (dorun (for [x (range *width*) y (range *height*)]
           (if (and (not (nil? (grid-at x y))) (< (:time (grid-at x y)) (- *time* 1)))
             (def *grid* (assoc *grid* (+ (* y *width*) x) nil))))))

(defn render [g]
  (let [img (new BufferedImage *gridpanel-width* *gridpanel-height*
		 (. BufferedImage TYPE_INT_ARGB))
        bg (. img (getGraphics))]
    (doto bg
      (.setRenderingHint (. RenderingHints KEY_ANTIALIASING)
			 (. RenderingHints VALUE_ANTIALIAS_ON))
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
    (str event)
    (str "!" event)))

(defn update-logs-panel []
  (. *true-events-box* setText
     (apply str (map str (filter #(<= (:time %) *time*) *true-events*))))
  (. *abducer-log-box* setText (apply str (map #(format "%s\n" %) *abducer-log*)))
  (. *strat-events-box*
     setText (apply str (map (fn [event]
			       (format-event event (filter #(<= (:time %) *time*)
							     *true-events*)))
		     (filter #(<= (:time %) *time*) *strat-events*))))
  (. *strat-log-box*
     setText (apply str (map str (filter #(<= (:time %) *time*) *strat-log*)))))

(defn next-step []
  (when (< *time* (:Steps *params*))
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
	strategy (get-strategy)
	{te :trueevents ss :stratstate sensors :sensors results :results}
	(tracking/run params (init-strat-state strategy (EventLog. #{} #{})))]
    (def *params* params)
    (def *abducer-log* (:abducer-log ss))
    (def *true-events* (get-events te))
    (def *strat-log* (:log ss))
    (def *strat-events* (get-events (:problem-data ss)))
    (def *time* 0)
    (def *width* (:GridWidth results))
    (def *height* (:GridHeight results))
    (def *grid-cell-width* (/ *gridpanel-width* *width*))
    (def *grid-cell-height* (/ *gridpanel-height* *height*))
    (def *sensors* sensors)
    (update-logs-panel)
    (. *gridpanel* (repaint))
    (. *steplabel* (setText (str "Step: " *time*)))
    (. *resultslabel* (setText (format "Correct: %.2f%%" (evaluate te ss))))))

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
	:gridx 0, :gridy 0, :gridheight 14
	*gridpanel*

	:gridx 1, :gridy 0, :gridheight 1
	(JLabel. "Steps:")
	:gridx 2, :gridy 0
	(:Steps *param-spinners*)

	:gridx 1, :gridy 1
	(JLabel. "NumberEntities:")
	:gridx 2, :gridy 1
	(:NumberEntities *param-spinners*)

	:gridx 1, :gridy 2
	(JLabel. "MaxWalk:")
	:gridx 2, :gridy 2
	(:MaxWalk *param-spinners*)

	:gridx 1, :gridy 3
	(JLabel. "ProbNewEntities:")
	:gridx 2, :gridy 3
	(:ProbNewEntities *param-spinners*)

	:gridx 1, :gridy 4
	(JLabel. "SensorReportNoise:")
	:gridx 2, :gridy 4
	(:SensorReportNoise *param-spinners*)

	:gridx 1, :gridy 5
	(JLabel. "BeliefNoise:")
	:gridx 2, :gridy 5
	(:BeliefNoise *param-spinners*)

	:gridx 1, :gridy 6
	(JLabel. "GridWidth:")
	:gridx 2, :gridy 6
	(:GridWidth *param-spinners*)

	:gridx 1, :gridy 7
	(JLabel. "GridHeight:")
	:gridx 2, :gridy 7
	(:GridHeight *param-spinners*)

	:gridx 1, :gridy 8
	(JLabel. "SensorCoverage:")
	:gridx 2, :gridy 8
	(:SensorCoverage *param-spinners*)

	:gridx 1, :gridy 9
	(JLabel. "Strategy:")
	:gridx 2, :gridy 9
	(:Strategy *param-other*)

	:gridx 1, :gridy 10
	*newbutton*
	:gridx 2, :gridy 10
	*steplabel*

	:gridx 1, :gridy 11
	*prevbutton*
	:gridx 2, :gridy 11
	*nextbutton*

	:gridx 1, :gridy 12
	*resultslabel*

	:gridx 2, :gridy 12
	*mouse-xy*

	:gridy 13, :gridwidth 2, :gridheight :REMAINDER
	(JPanel.))))

(def *logspanel*
     (doto (JPanel. (GridBagLayout.))
       (grid-bag-layout
	:fill :BOTH, :insets (Insets. 5 5 5 5)

	:gridx 0, :gridy 0
	(JLabel. "True log")
	:gridx 1, :gridy 0
	(JLabel. "Strategy log")

	:gridx 0, :gridy 1
	(JScrollPane. *abducer-log-box*)
	:gridx 1, :gridy 1
	(JScrollPane. *strat-log-box*)
	
	:gridx 0, :gridy 2
	(JLabel. "True events")
	:gridx 1, :gridy 2
	(JLabel. "Strategy events")
	
	:gridx 0, :gridy 3
	(JScrollPane. *true-events-box*)
	:gridx 1, :gridy 3
	(JScrollPane. *strat-events-box*))))

(defn start-player []
  (doto (JFrame. "Tracking player")
    (.setContentPane *mainpanel*)
    (.setResizable false)
    (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.pack)
    (.show))
  (doto (JFrame. "Logs")
    (.setContentPane *logspanel*)
    (.setResizable false)
    (.pack)
    (.show)))



