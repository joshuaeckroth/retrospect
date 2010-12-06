(ns simulator.player.gui
  (:import (java.awt GridBagLayout Insets Dimension))
  (:import (java.awt.image BufferedImage))
  (:import (javax.swing JPanel JFrame JButton JTextField JTextArea
			JLabel JScrollPane JSpinner SpinnerNumberModel JComboBox
                        ImageIcon Scrollable JViewport))
  (:use [simulator.player.state])
  (:use [simulator.strategies :only [strategies init-one-run-state run-simulation-step]])
  (:use [simulator.epistemicstates :only [draw-ep-state-tree]]))

(def *mainframe* nil)
(def *problem-diagram* nil)
(def *problem-params-panel* nil)
(def *problem-stats-panel* nil)

(def *steplabel* (JLabel. "Step: "))
(def *false-pos-label* (JLabel. "False pos: "))
(def *false-neg-label* (JLabel. "False neg: "))

(def *ep-tree* (BufferedImage. 1 1 (. BufferedImage TYPE_4BYTE_ABGR)))
(def *truedata-log-box* (JTextArea. 10 50))
(def *abduction-log-box* (JTextArea. 10 50))
(def *problem-log-box* (JTextArea. 10 50))

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


(def *param-spinners*
  {:Steps (JSpinner. (SpinnerNumberModel. 50 1 1000 1))
   :SensorReportNoise (JSpinner. (SpinnerNumberModel. 0 0 100 10))
   :BeliefNoise (JSpinner. (SpinnerNumberModel. 0 0 100 10))})

(def *param-combobox*
  {:Strategy {:elements strategies :combobox (JComboBox. (to-array strategies))}})

(defn get-params
  []
  (apply hash-map
         (flatten
          (concat
           ((:get-params-fn (:player-fns *problem*)))
           (for [k (keys *param-spinners*)]
             [k (->> (k *param-spinners*)
                     .getModel .getNumber .intValue)])
           (for [k (keys *param-combobox*)]
             [k (nth (:elements (k *param-combobox*))
                     (.getSelectedIndex (:combobox (k *param-combobox*))))])))))

(defn update-logs-panel
  []
  )

(defn render-ep-tree-diagram
  [g]
  (. g (drawImage *ep-tree* 0 0 nil)))

(def *ep-tree-panel*
  (doto (proxy [JViewport Scrollable] []
          (paint [g]
            (render-ep-tree-diagram g))
          (getPreferredSize []
            (new Dimension (.getWidth *ep-tree*) (.getHeight *ep-tree*)))
          (getPreferredScrollableViewportSize []
            (new Dimension (.getWidth *ep-tree*) (.getHeight *ep-tree*)))
          (getScrollableBlockIncrement [visibleRect orientation direction]
            1)
          (getScrollableUnitIncrement [visibleRect orientation direction]
            1)
          (getScrollableTracksViewportWidth [] false)
          (getScrollableTracksViewportHeight [] false))))

(defn get-ep-tree-viewport
  []
  (doto (JViewport.)
    (.setView (JLabel. (ImageIcon. *ep-tree*)))))

(def *ep-tree-scrollpane*
  (doto
      (JScrollPane. (get-ep-tree-viewport))
    (.setPreferredSize
     (new Dimension 600 600))))

(defn update-ep-tree-diagram
  []
  (def *ep-tree* (draw-ep-state-tree (:ep-state-tree *or-state*)))
  (. *ep-tree-scrollpane* (setViewport (get-ep-tree-viewport))))

(defn step []
  (let [or-state (run-simulation-step *problem* *truedata* *or-state* *params* 0)]
    (update-or-state or-state)
    (update-time (:time (:ep-state or-state)))
    (update-ep-tree-diagram)
    ((:update-diagram-fn (:player-fns *problem*)))
    ((:update-stats-fn (:player-fns *problem*)))
    (update-logs-panel)
    (. *truedata-log-box* setText ((:update-truedata-log-box-fn (:player-fns *problem*))))
    (. *problem-log-box* setText ((:update-problem-log-box-fn (:player-fns *problem*))))))

(defn next-step []
  (when (< *time* (:Steps *params*))
    (update-time (inc *time*)))
  (step))

(defn prev-step []
  (when (> *time* 0)
    (update-time (dec *time*)))
  (step))

(defn choose-ep-state
  [id]
)

(defn new-simulation
  []
  (let [params (get-params)
        meta? (:MetaAbduce params)
	strategy (:Strategy params)
        sensors ((:sensor-gen-fn *problem*) params)
        truedata ((:truedata-fn *problem*) params)
        or-state (init-one-run-state
                  strategy sensors (:initial-problem-data *problem*) meta?)]
    (update-params params)
    (update-sensors sensors)
    (update-truedata truedata)
    (update-or-state or-state)
    (choose-ep-state "A")))

(def *newbutton*
     (let [b (JButton. "New")]
       (with-action b e (new-simulation))
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

(def *params-panel*
     (doto (JPanel. (GridBagLayout.))
       (grid-bag-layout
	:fill :BOTH, :insets (Insets. 5 5 5 5)

	:gridx 0, :gridy 0
	(JLabel. "Steps:")
	:gridx 1, :gridy 0
	(:Steps *param-spinners*)

	:gridx 0, :gridy 1
	(JLabel. "SensorReportNoise:")
	:gridx 1, :gridy 1
	(:SensorReportNoise *param-spinners*)

	:gridx 0, :gridy 2
	(JLabel. "BeliefNoise:")
	:gridx 1, :gridy 2
	(:BeliefNoise *param-spinners*)

	:gridx 0, :gridy 3
	(JLabel. "Strategy:")
	:gridx 1, :gridy 3
	(:combobox (:Strategy *param-combobox*)))))

(def *stats-panel*
     (doto (JPanel. (GridBagLayout.))
       (grid-bag-layout
	:fill :BOTH, :insets (Insets. 5 5 5 5)

	:gridx 0, :gridy 0
	(JLabel. "Step:"))))

(defn get-mainframe
  []
  (doto (JPanel. (GridBagLayout.))
    (grid-bag-layout
     :fill :BOTH, :insets (Insets. 5 5 5 5)
     
     :gridx 0, :gridy 0, :gridheight 7
     *problem-diagram*

     :gridx 1, :gridy 0, :gridheight 1
     *params-panel*

     :gridx 1, :gridy 1
     *problem-params-panel*

     :gridx 1, :gridy 2
     *newbutton*
     :gridx 2, :gridy 2
     *steplabel*
     
     :gridx 1, :gridy 3
     *prevbutton*
     
     :gridx 1, :gridy 4
     *nextbutton*

     :gridx 1, :gridy 5
     *stats-panel*

     :gridx 1, :gridy 6
     *problem-stats-panel*

     :gridy 7, :gridheight :REMAINDER
     (JPanel.))))

(def *ep-tree-frame*
  (doto (JPanel. (GridBagLayout.))
    (grid-bag-layout
     :fill :BOTH, :insets (Insets. 5 5 5 5)

     :gridx 0, :gridy 0
     *ep-tree-scrollpane*)))

(def *logs-frame*
  (doto (JPanel. (GridBagLayout.))
    (grid-bag-layout
     :fill :BOTH, :insets (Insets. 5 5 5 5)

     :gridx 0, :gridy 0
     (JLabel. "True data log")
     :gridx 0, :gridy 1
     (JScrollPane. *truedata-log-box*)
     
     :gridx 0, :gridy 2
     (JLabel. "Abduction log")
     :gridx 0, :gridy 3
     (JScrollPane. *abduction-log-box*)

     :gridx 0, :gridy 4
     (JLabel. "Problem log")
     :gridx 0, :gridy 5
     (JScrollPane. *problem-log-box*))))

(defn start-player
  [problem]

  (update-problem problem)
  (update-params (get-params))

  (def *problem-diagram* ((:get-diagram-fn (:player-fns problem)) 600 600))
  (def *problem-params-panel* ((:get-params-panel-fn (:player-fns problem))))
  (def *problem-stats-panel* ((:get-stats-panel-fn (:player-fns problem))))

  (def *mainframe* (get-mainframe))
  
  (doto (JFrame. "Epistemic state tree")
    (.setContentPane *ep-tree-frame*)
    (.setResizable false)
    (.pack)
    (.show))
  (doto (JFrame. "Logs")
    (.setContentPane *logs-frame*)
    (.setResizable false)
    (.pack)
    (.show))
  (doto (JFrame. "Player")
    (.setContentPane *mainframe*)
    (.setResizable false)
    (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.pack)
    (.show)))

