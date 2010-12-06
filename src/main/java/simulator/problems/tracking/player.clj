(ns simulator.problems.tracking.player
  (:import (java.awt Color Graphics2D Dimension GridBagLayout Insets RenderingHints))
  (:import (java.awt.image BufferedImage))
  (:import (javax.swing JPanel JFrame JButton JTextField JTextArea
			JLabel JScrollPane JSpinner SpinnerNumberModel JComboBox))
  (:require [simulator.problems.tracking eventlog])
  (:import [simulator.problems.tracking.eventlog EventLog])
  (:use [clojure.contrib.math :only [floor]])
  (:use [simulator.problems.tracking.sensors :only (sees)])
  (:use [simulator.problems.tracking.eventlog :only (get-events get-entities)])
  (:use [simulator.player.state]))

(def *diagram* nil)
(def *diagram-width* nil)
(def *diagram-height* nil)
(def *width* nil)
(def *height* nil)
(def *grid-cell-width* nil)
(def *grid-cell-height* nil)
(def *grid* nil)

(def *percent-events-correct-label* (JLabel.))
(def *percent-identities-correct-label* (JLabel.))
(def *mouse-xy* (JLabel.))

;; from http://stuartsierra.com/2010/01/08/agents-of-swing
(defmacro with-action [component event & body]
  `(. ~component addActionListener
      (proxy [java.awt.event.ActionListener] []
        (actionPerformed [~event] ~@body))))

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


(def *problem-param-spinners*
     {:NumberEntities (JSpinner. (SpinnerNumberModel. 1 1 100 1))
      :MaxWalk (JSpinner. (SpinnerNumberModel. 1 1 100 1))
      :ProbNewEntities (JSpinner. (SpinnerNumberModel. 0 0 100 10))
      :ProbMovement (JSpinner. (SpinnerNumberModel. 50 0 100 10))
      :GridWidth (JSpinner. (SpinnerNumberModel. 10 1 100 1))
      :GridHeight (JSpinner. (SpinnerNumberModel. 10 1 100 1))
      :SensorCoverage (JSpinner. (SpinnerNumberModel. 100 0 100 10))})

(defn player-get-params
  []
  (for [k (keys *problem-param-spinners*)]
    [k (->> (k *problem-param-spinners*)
            .getModel .getNumber .intValue)]))

(defn player-get-params-panel
  []
  (doto (JPanel. (GridBagLayout.))
    (grid-bag-layout
     :fill :BOTH, :insets (Insets. 5 5 5 5)
     
     :gridx 0, :gridy 0
     (JLabel. "NumberEntities:")
     :gridx 1, :gridy 0
     (:NumberEntities *problem-param-spinners*)
     
     :gridx 0, :gridy 1
     (JLabel. "MaxWalk:")
     :gridx 1, :gridy 1
     (:MaxWalk *problem-param-spinners*)
     
     :gridx 0, :gridy 2
     (JLabel. "ProbNewEntities:")
     :gridx 1, :gridy 2
     (:ProbNewEntities *problem-param-spinners*)
     
     :gridx 0, :gridy 3
     (JLabel. "ProbMovement:")
     :gridx 1, :gridy 3
     (:ProbMovement *problem-param-spinners*)
     
     :gridx 0, :gridy 4
     (JLabel. "GridWidth:")
     :gridx 1, :gridy 4
     (:GridWidth *problem-param-spinners*)
     
     :gridx 0, :gridy 5
     (JLabel. "GridHeight:")
     :gridx 1, :gridy 5
     (:GridHeight *problem-param-spinners*)
     
     :gridx 0, :gridy 6
     (JLabel. "SensorCoverage:")
     :gridx 1, :gridy 6
     (:SensorCoverage *problem-param-spinners*))))

(defn sensors-see [x y]
  (some #(sees % x y) *sensors*))

(defn get-grid-from-xy [x y]
  [(floor (/ x *grid-cell-width*)) (floor (/ y *grid-cell-height*))])

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
     (let [e (grid-at x y)]
       (when (not (nil? e))
	 (cond
          (= (type e) simulator.problems.tracking.entities.Entity)
          (fill-cell g x y (new Color 150 150 150 150))
          (= (type e) simulator.problems.tracking.events.EventNew)
          (fill-cell g x y (new Color 180 180 255 150))
          (= (type e) simulator.problems.tracking.events.EventMove)
          (do
            (fill-cell g (:x (:oldpos e)) (:y (:oldpos e))
                       (new Color 150 150 150 150))
            (fill-cell g x y (new Color 100 100 100 150))
            (draw-move g (:x (:oldpos e)) (:y (:oldpos e)) x y
                       (new Color 0 0 0)))))))))

(defn update-grid []
  (def *grid* (vec (repeat (* *width* *height*) nil)))
  (let [true-entities (get-entities (:eventlog (get *truedata* *time*)))
        true-events (get-events (:eventlog (get *truedata* *time*)))]
    (when true-entities
      (dorun (for [e (filter #(<= (:time (first (:snapshots %))) *time*) true-entities)]
               (let [{x :x y :y} (:pos (last (filter #(<= (:time %) *time*)
                                                     (:snapshots e))))]
                 (def *grid* (assoc *grid* (+ (* y *width*) x) e))))))
    (when true-events
      (dorun (for [e (filter #(= (:time %) *time*) true-events)]
               (let [{x :x y :y}
                     (if (= (type e) simulator.problems.tracking.events.EventMove)
                       (:newpos e) (:pos e))]
                 (def *grid* (assoc *grid* (+ (* y *width*) x) e))))))))

(defn render [g]
  (let [img (new BufferedImage *diagram-width* *diagram-height*
		 (. BufferedImage TYPE_INT_ARGB))
        bg (. img (getGraphics))]
    (doto bg
      (.setRenderingHint (. RenderingHints KEY_ANTIALIASING)
			 (. RenderingHints VALUE_ANTIALIAS_ON))
      (.setColor (. Color white))
      (.fillRect 0 0 *diagram-width* *diagram-height*))
    (update-grid)
    (draw-grid bg)
    (. g (drawImage img 0 0 nil))
    (. bg (dispose))))

(defn player-update-diagram
  []
  (def *width* (:GridWidth *params*))
  (def *height* (:GridHeight *params*))
  (def *grid-cell-width* (/ *diagram-width* *width*))
  (def *grid-cell-height* (/ *diagram-height* *height*))
  (. *diagram* (repaint)))
    
(defn player-get-diagram
  [diagram-width diagram-height]
  (def *diagram-width* diagram-width)
  (def *diagram-height* diagram-height)
  (def *diagram*
    (doto (proxy [JPanel] []
            (paint [g] (render g)))
      (.setPreferredSize
       (new Dimension *diagram-width* *diagram-height*))
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
  (player-update-diagram)
  *diagram*)

(defn player-get-stats-panel
  []
  (doto (JPanel. (GridBagLayout.))
    (grid-bag-layout
     :fill :BOTH, :insets (Insets. 5 5 5 5)
     
     :gridx 0, :gridy 0
     (JLabel. "PercentEventsCorrect:")
     :gridx 1, :gridy 0
     *percent-events-correct-label*
     
     :gridx 0, :gridy 1
     (JLabel. "PercentIdentitiesCorrect:")
     :gridx 1, :gridy 1
     *percent-identities-correct-label*

     :gridx 0, :gridy 2
     *mouse-xy*)))

(defn player-update-stats
  []
  (. *percent-events-correct-label*
     (setText
      (format "%.2f%%"
              (:PercentEventsCorrect (get (:results *or-state*) (dec *time*))))))
  (. *percent-identities-correct-label*
     (setText
      (format "%.2f%%"
              (:PercentIdentitiesCorrect (get (:results *or-state*) (dec *time*)))))))

(defn format-event
  [event true-events]
  (if (some #(= event %) true-events)
    (str event)
    (str "!" event)))

(defn player-update-truedata-log-box
  []
  (let [events (filter #(<= (:time %) *time*)
                       (get-events (:eventlog (get *truedata* *time*))))]
    (apply str (interpose "\n" (map str events)))))

(defn player-update-problem-log-box
  [])
