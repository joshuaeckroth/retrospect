(ns simulator.player.gui
  (:import (java.awt GridBagLayout Insets Dimension Checkbox))
  (:import (java.awt.image BufferedImage))
  (:import (javax.swing JPanel JFrame JButton JTextField JTextArea
			JLabel JScrollPane JSpinner SpinnerNumberModel JComboBox
                        ImageIcon Scrollable JViewport JTabbedPane))
  (:use [simulator.player.state])
  (:use [simulator.strategies :only [strategies init-one-run-state run-simulation-step]])
  (:use [simulator.epistemicstates :only [draw-ep-state-tree list-ep-states
                                          current-ep-state goto-ep-state
                                          previous-ep-state]]))

(def *mainframe* nil)
(def *problem-diagram* nil)
(def *problem-params-panel* nil)
(def *problem-stats-panel* nil)

(def *steplabel* (JLabel. "Step: "))
(def *false-pos-label* (JLabel. "False pos: "))
(def *false-neg-label* (JLabel. "False neg: "))
(def *goto-ep-state-combobox* (JComboBox. (to-array "")))

(def *ep-tree* (BufferedImage. 1 1 (. BufferedImage TYPE_4BYTE_ABGR)))
(def *truedata-log-box* (JTextArea. 10 50))
(def *abduction-log-label* (JLabel. "Abduction log"))
(def *abduction-log-box* (JTextArea. 10 50))
(def *problem-log-label* (JLabel. "Problem log"))
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

(def *param-checkbox*
  {:MetaAbduce (Checkbox.)})

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
                     (.getSelectedIndex (:combobox (k *param-combobox*))))])
           (for [k (keys *param-checkbox*)]
             [k (->> (k *param-checkbox*)
                     .getState)])))))

(defn render-ep-tree-diagram
  [g]
  (. g (drawImage *ep-tree* 0 0 nil)))

(def *ep-tree-panel*
  (doto (proxy [JViewport Scrollable] []
          (paint [g]
            (render-ep-tree-diagram g))
          (getPreferredSize []
            (Dimension. (.getWidth *ep-tree*) (.getHeight *ep-tree*)))
          (getPreferredScrollableViewportSize []
            (Dimension. (.getWidth *ep-tree*) (.getHeight *ep-tree*)))
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

(defn update-goto-ep-state-combobox
  []
  (loop [cb (doto *goto-ep-state-combobox* (.removeAllItems))
         ep-states (list-ep-states (:ep-state-tree *or-state*))]
    (if (empty? ep-states)
      (def *goto-ep-state-combobox*
        (doto cb (.setSelectedItem (str (:ep-state *or-state*)))))
      (recur (doto cb (.addItem (first ep-states))) (rest ep-states)))))

(defn update-logs
  []
  (. *truedata-log-box* setText ((:update-truedata-log-box-fn (:player-fns *problem*))))
  (let [ep-state (previous-ep-state (:ep-state-tree *or-state*))]
    (. *problem-log-label* setText (format "Problem log for: %s" (str ep-state)))
    (. *problem-log-box* setText
       (apply str (interpose "\n" (map str (:log ep-state)))))
    (. *abduction-log-label* setText (format "Abduction log for: %s" (str ep-state)))
    (. *abduction-log-box* setText
       (apply str (interpose "\n" (map str (:abducer-log ep-state)))))))

(defn update-everything
  [or-state]
  (update-or-state or-state)
  (let [ep-state (previous-ep-state (:ep-state-tree *or-state*))]
    (if-not (nil? ep-state)
      (do
        (update-time (:time ep-state))
        (. *steplabel* (setText (format "Step: %d" *time*))))
      (do
        (update-time -1)
        (. *steplabel* (setText "Step: N/A"))))
    (update-goto-ep-state-combobox)
    (update-ep-tree-diagram)
    ((:update-diagram-fn (:player-fns *problem*)))
    ((:update-stats-fn (:player-fns *problem*)))
    (update-logs)))

(defn goto-ep-state-action
  []
  (let [id (re-find #"^[A-Z]+" (. *goto-ep-state-combobox* (getSelectedItem)))
        ep-state-tree (goto-ep-state (:ep-state-tree *or-state*) id)]
    (if-not (nil? ep-state-tree)
      (update-everything
       (-> *or-state*
           (assoc :ep-state-tree ep-state-tree)
           (assoc :ep-state (current-ep-state ep-state-tree)))))))

(defn step
  []
  (let [or-state (run-simulation-step *problem* *truedata* *or-state* *params* 0)]
    (update-everything or-state)))

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
    (update-everything or-state)))

(def *newbutton*
  (let [b (JButton. "New")]
    (with-action b e (new-simulation))
    b))

(defn next-step
  []
  (when (< *time* (:Steps *params*))
    (step)))

(def *nextbutton*
  (let [b (JButton. "Next Step")]
    (with-action b e (next-step))
    b))

(def *goto-ep-state-button*
  (let [b (JButton. "Goto ep-state")]
    (with-action b e (goto-ep-state-action))
    b))

(def *params-panel*
  (doto (JPanel. (GridBagLayout.))
    (grid-bag-layout
     :fill :BOTH, :insets (Insets. 5 5 5 5)
     
     :gridx 0, :gridy 0
     (JLabel. "Strategy:")
     :gridx 0, :gridy 1, :gridwidth 2
     (:combobox (:Strategy *param-combobox*))

     :gridx 0, :gridy 2, :gridwidth 1
     (JLabel. "Steps:")
     :gridx 1, :gridy 2
     (:Steps *param-spinners*)
     
     :gridx 0, :gridy 3
     (JLabel. "SensorReportNoise:")
     :gridx 1, :gridy 3
     (:SensorReportNoise *param-spinners*)
     
     :gridx 0, :gridy 4
     (JLabel. "BeliefNoise:")
     :gridx 1, :gridy 4
     (:BeliefNoise *param-spinners*)

     :gridx 0, :gridy 5
     (JLabel. "MetaAbduce:")
     :gridx 1, :gridy 5
     (:MetaAbduce *param-checkbox*))))

(def *stats-panel*
     (doto (JPanel. (GridBagLayout.))
       (grid-bag-layout
	:fill :BOTH, :insets (Insets. 5 5 5 5)

	:gridx 0, :gridy 0
        *steplabel*)))

(def *ep-tree-panel*
  (doto (JPanel. (GridBagLayout.))
    (grid-bag-layout
     :fill :BOTH, :insets (Insets. 5 5 5 5)

     :gridx 0, :gridy 0
     *ep-tree-scrollpane*)))

(def *logs-panel*
  (doto (JPanel. (GridBagLayout.))
    (grid-bag-layout
     :fill :BOTH, :insets (Insets. 5 5 5 5)

     :gridx 0, :gridy 0
     (JLabel. "True data log")
     :gridx 0, :gridy 1
     (JScrollPane. *truedata-log-box*)
     
     :gridx 0, :gridy 2
     *abduction-log-label*
     :gridx 0, :gridy 3
     (JScrollPane. *abduction-log-box*)

     :gridx 0, :gridy 4
     *problem-log-label*
     :gridx 0, :gridy 5
     (JScrollPane. *problem-log-box*))))

(defn get-mainframe
  []
  (doto (JFrame. "Player")
    (.setLayout (GridBagLayout.))
    (grid-bag-layout
     :fill :BOTH, :insets (Insets. 5 5 5 5)
     
     :gridx 0, :gridy 0, :gridheight 6
     (doto (JTabbedPane.)
       (.addTab "Problem diagram" *problem-diagram*)
       (.addTab "Epistemic state tree" *ep-tree-panel*)
       (.addTab "Logs" *logs-panel*)
       (.setSelectedIndex 0))
     
     :gridx 1, :gridy 0, :gridheight 1, :gridwidth 2
     *params-panel*

     :gridx 1, :gridy 1
     *problem-params-panel*

     :gridx 1, :gridy 2, :gridwidth 1
     *newbutton*
     :gridx 2, :gridy 2
     *nextbutton*

     :gridx 1, :gridy 3
     *goto-ep-state-combobox*
     :gridx 2, :gridy 3
     *goto-ep-state-button*

     :gridx 1, :gridy 4, :gridwidth 2
     *stats-panel*

     :gridx 1, :gridy 5
     *problem-stats-panel*

     :gridy 6, :gridheight :REMAINDER
     (JPanel.))))

(defn start-player
  [problem]

  (update-problem problem)
  (update-params (get-params))

  (def *problem-diagram* ((:get-diagram-fn (:player-fns problem)) 600 600))
  (def *problem-params-panel* ((:get-params-panel-fn (:player-fns problem))))
  (def *problem-stats-panel* ((:get-stats-panel-fn (:player-fns problem))))

  (def *mainframe* (get-mainframe))
  
  (doto *mainframe*
    (.setResizable true)
    (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.pack)
    (.show))

  (new-simulation))

