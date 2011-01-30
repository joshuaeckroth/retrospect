(ns samre.problems.tracking.player
  (:import (java.awt Graphics2D Dimension GridBagLayout
                     Insets RenderingHints BasicStroke))
  (:import (java.awt.image BufferedImage))
  (:import (javax.swing JPanel JFrame JButton JTextField JTextArea
			JLabel JScrollPane JSpinner SpinnerNumberModel JComboBox))
  (:require [samre.problems.tracking eventlog])
  (:import [samre.problems.tracking.eventlog EventLog])
  (:use [clojure.contrib.math :only [floor]])
  (:use [samre.problems.tracking.sensors :only (sees)])
  (:use [samre.problems.tracking.eventlog :only (get-events get-entities)])
  (:use [samre.problems.tracking.entities :only (pair-snapshots)])
  (:use [samre.colors])
  (:use [samre.player.state]))

(def *grid* nil)
(def *diagram-width* nil)
(def *diagram-height* nil)
(def *grid-width* nil)
(def *grid-height* nil)
(def *grid-cell-width* nil)
(def *grid-cell-height* nil)

(def *percent-events-correct-label* (JLabel.))
(def *percent-events-wrong-label* (JLabel.))
(def *percent-identities-correct-label* (JLabel.))
(def *mouse-xy* (JLabel. "Grid ?, ?"))

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
      :SensorCoverage (JSpinner. (SpinnerNumberModel. 100 0 100 10))
      :SensorSeesColor (JSpinner. (SpinnerNumberModel. 100 0 100 10))})

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
     (:SensorCoverage *problem-param-spinners*)

     :gridx 0, :gridy 7
     (JLabel. "SensorSeesColor:")
     :gridx 1, :gridy 7
     (:SensorSeesColor *problem-param-spinners*))))

(defn get-grid-from-xy [x y]
  [(floor (/ x *grid-cell-width*)) (floor (/ y *grid-cell-height*))])

(defn grid-at [x y]
  (nth *grid* (+ (* y *grid-width*) x)))

(defn fill-cell [#^Graphics2D g x y c]
  (doto g
    (.setColor c)
    (.fillRect (* x *grid-cell-width*) (* y *grid-cell-height*)
	       *grid-cell-width* *grid-cell-height*)))

(defn draw-move [#^Graphics2D g oldx oldy newx newy color width]
  (let [oldpx (+ (* oldx *grid-cell-width*) (/ *grid-cell-width* 2))
	oldpy (+ (* oldy *grid-cell-height*) (/ *grid-cell-height* 2))
	newpx (+ (* newx *grid-cell-width*) (/ *grid-cell-width* 2))
	newpy (+ (* newy *grid-cell-height*) (/ *grid-cell-height* 2))]
    (doto g
      (.setColor color)
      (.setStroke (BasicStroke. width))
      (.drawLine oldpx oldpy newpx newpy))))

(defn draw-movements [#^Graphics2D g entity]
  (let [in-range (fn [s] (and (<= (:time s) *time*) (> (:time s) *time-prev*)))
        pairs (filter #(in-range (second %))
                      (pair-snapshots entity))]
    (doseq [p pairs]
      (let [degree (int (* 255 (/ (- *time* (:time (second p))) (- *time* *time-prev*))))
            width (double (+ 2 (* 5 (/ degree 255))))]
        (draw-move g (:x (:pos (first p))) (:y (:pos (first p)))
                   (:x (:pos (second p))) (:y (:pos (second p)))
                   (var-color degree)
                   width)))))

(defn draw-grid [g]
  (dorun
   (for [x (range *grid-width*) y (range *grid-height*)]
     (fill-cell g x y white)))
  (dorun
   (for [x (range *grid-width*) y (range *grid-height*)]
     (doseq [sensor *sensors*]
       (when (sees sensor x y)
         (if (:sees-color (:attributes sensor))
           (fill-cell g x y yellow-alpha)
           (fill-cell g x y gray-alpha))))))
  (dorun
   (for [x (range *grid-width*) y (range *grid-height*)]
     (when-let [e (grid-at x y)]
       (fill-cell g x y (:color e))
       (draw-movements g e)))))

(defn update-grid []
  (def *grid* (vec (repeat (* *grid-width* *grid-height*) nil)))
  (let [true-entities (get-entities (:eventlog (get *truedata* *time*)))]
    (when true-entities
      (dorun (for [e (filter #(<= (:time (first (:snapshots %))) *time*) true-entities)]
               (let [{x :x y :y} (:pos (last (filter #(<= (:time %) *time*)
                                                     (:snapshots e))))]
                 (def *grid* (assoc *grid* (+ (* y *grid-width*) x) e))))))))

(defn render [g]
  (def *diagram-width* (.. g (getClipBounds) width))
  (def *diagram-height* (.. g (getClipBounds) height))
  (def *grid-width* (:GridWidth *params*))
  (def *grid-height* (:GridHeight *params*))
  (def *grid-cell-width* (/ *diagram-width* *grid-width*))
  (def *grid-cell-height* (/ *diagram-height* *grid-height*))
  (let [img (new BufferedImage *diagram-width* *diagram-height*
                 (. BufferedImage TYPE_INT_ARGB))
        bg (. img (getGraphics))]
    (doto bg
      (.setRenderingHint (. RenderingHints KEY_ANTIALIASING)
                         (. RenderingHints VALUE_ANTIALIAS_ON))
      (.setColor white)
      (.fillRect 0 0 *diagram-width* *diagram-height*))
    (update-grid)
    (draw-grid bg)
    (. g (drawImage img 0 0 nil))
    (. bg (dispose))))

(defn player-get-diagram
  []
  (doto (proxy [JPanel] []
          (paint [g] (render g)))
    (.addMouseListener
     (proxy [java.awt.event.MouseListener] []
       (mouseClicked [e])
       (mouseEntered [e])
       (mouseExited [e]
         (. *mouse-xy* setText "Grid ?, ?"))
       (mousePressed [e])
       (mouseReleased [e])))
    (.addMouseMotionListener
     (proxy [java.awt.event.MouseMotionListener] []
       (mouseDragged [e])
       (mouseMoved [e]
         (. *mouse-xy* setText
            (let [[x y] (get-grid-from-xy (. e getX) (. e getY))]
              (format "Grid %d, %d" x y))))))))

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
     (JLabel. "PercentEventsWrong:")
     :gridx 1, :gridy 1
     *percent-events-wrong-label*
     
     :gridx 0, :gridy 2
     (JLabel. "PercentIdentitiesCorrect:")
     :gridx 1, :gridy 2
     *percent-identities-correct-label*

     :gridx 0, :gridy 3
     *mouse-xy*)))

(defn player-update-stats
  []
  (if (>= *time* 0)
    (let [t (int (/ *time* (:StepsBetween *params*)))]
      (. *percent-events-correct-label*
         (setText
          (format "%.2f%%"
                  (:PercentEventsCorrect (get (:results *or-state*) t)))))
      (. *percent-events-wrong-label*
         (setText
          (format "%.2f%%"
                  (:PercentEventsWrong (get (:results *or-state*) t)))))
      (. *percent-identities-correct-label*
         (setText
          (format "%.2f%%"
                  (:PercentIdentitiesCorrect (get (:results *or-state*) t))))))
    (do
      (. *percent-events-correct-label* (setText "N/A"))
      (. *percent-events-wrong-label* (setText "N/A"))
      (. *percent-identities-correct-label* (setText "N/A")))))

(defn format-event
  [event]
  (if (some #(= event %) (get-events (:eventlog (:problem-data (:ep-state *or-state*)))))
    (str event)
    (str "!" event)))

(defn player-update-truedata-log-box
  []
  (let [events (filter #(and (<= (:time %) *time*) (> (:time %) *time-prev*))
                       (get-events (:eventlog (get *truedata* *time*))))]
    (apply str (interpose "\n" (map format-event events)))))
