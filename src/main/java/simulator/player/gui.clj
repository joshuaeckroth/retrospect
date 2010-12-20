(ns simulator.player.gui
  (:import [misc WrapLayout])
  (:import (java.awt Color GridBagLayout FlowLayout Insets Dimension Checkbox))
  (:import (java.awt.image BufferedImage))
  (:import (javax.swing JPanel JFrame JButton JTextField JTextArea
			JLabel JScrollPane JSpinner SpinnerNumberModel JComboBox
                        ImageIcon JViewport Scrollable JTabbedPane JTable))
  (:import (java.util Vector))
  (:use [incanter.core :only [to-list with-data dataset nrow]])
  (:use [incanter.charts :only [scatter-plot]])
  (:use [simulator.problem :only [get-headers]])
  (:use [simulator.player.state])
  (:use [simulator.onerun :only [init-one-run-state run-simulation-step]])
  (:use [simulator.epistemicstates :only [draw-ep-state-tree list-ep-states
                                          current-ep-state goto-ep-state
                                          root-ep-state?
                                          previous-ep-state]]))

(def *mainframe* nil)
(def *problem-diagram* nil)
(def *problem-params-panel* nil)
(def *problem-stats-panel* nil)

(def *steplabel* (JLabel. "Step: "))
(def *goto-ep-state-combobox* (JComboBox. (to-array "")))

(def *ep-tree* (BufferedImage. 1 1 (. BufferedImage TYPE_4BYTE_ABGR)))
(def *truedata-log-box* (JTextArea.))
(def *abduction-log-label* (JLabel. "Abduction log"))
(def *abduction-log-box* (JTextArea.))
(def *meta-log-box* (JTextArea.))
(def *problem-log-label* (JLabel. "Problem log"))
(def *problem-log-box* (JTextArea.))
(def *results-checkboxes* {})
(def *results-checkboxes-panel* (JPanel.))

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

(def *param-checkbox*
  {:MetaAbduction (Checkbox.)})

(defn get-params
  []
  (apply hash-map
         (flatten
          (concat
           ((:get-params-fn (:player-fns *problem*)))
           (for [k (keys *param-spinners*)]
             [k (->> (k *param-spinners*)
                     .getModel .getNumber .intValue)])
           (for [k (keys *param-checkbox*)]
             [k (.getState (k *param-checkbox*))])))))

(defn get-ep-tree-viewport
  []
  (doto (JViewport.)
    (.setBackground Color/white)
    (.setView (JLabel. (ImageIcon. *ep-tree*)))))

(def *ep-tree-scrollpane*
  (JScrollPane. (get-ep-tree-viewport)))

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
       (apply str (interpose "\n" (map str (:abducer-log (:workspace ep-state))))))
    (. *meta-log-box* setText
       (apply str (interpose "\n---\n" (map (fn [ls] (apply str (interpose "\n" (map str ls))))
                                          (:meta-log *or-state*)))))))

(defn get-results-viewport
  []
  (let [headers (filter (fn [h] (if-let [cb (get *results-checkboxes* h)]
                                  (.getState cb)))
                        (get-headers *problem*))
        results-matrix (map (fn [r] (map (fn [h] (h r)) headers))
                            (:results *or-state*))]
    (doto (JViewport.)
      (.setView  (JTable. (Vector. (map #(Vector. %) (to-list results-matrix)))
                          (Vector. (map name headers)))))))

(def *results-scrollpane*
  (JScrollPane. (get-results-viewport)))

(defn update-results
  []
  (. *results-scrollpane* (setViewport (get-results-viewport))))

(defn get-results-checkboxes
  []
  (reduce (fn [m h] (let [cb (Checkbox. (name h) true)]
                      (. cb addItemListener
                         (proxy [java.awt.event.ItemListener] []
                           (itemStateChanged [_] (update-results))))
                      (assoc m h cb)))
          {} (get-headers *problem*)))

(defn get-results-checkboxes-panel
  []
  (loop [p (JPanel. (WrapLayout. FlowLayout/LEFT))
         cbs (get-headers *problem*)]
    (if (empty? cbs) p
        (recur (doto p (.add (get *results-checkboxes* (first cbs)))) (rest cbs)))))

(defn results-to-dataset
  []
  (dataset (get-headers *problem*)
           (map (fn [r] (map (fn [h] (get r h))
                             (get-headers *problem*)))
                (:results *or-state*))))

(def *results-graph-x-dropdown* (JComboBox.))

(defn get-results-graph-x-dropdown
  []
  (def *results-graph-x-dropdown*
    (JComboBox. (to-array (map name (get-headers *problem*))))))

(def *results-graph-y-dropdown* (JComboBox.))

(defn get-results-graph-y-dropdown
  []
  (def *results-graph-y-dropdown*
    (JComboBox. (to-array (map name (get-headers *problem*))))))

(def *results-graph* nil)

(defn paint-results-graph
  [g]
  (if *results-graph*
    (.draw *results-graph* g (.getClipBounds g))))

(def *results-graph-panel*
  (proxy [JPanel] []
    (getPreferredSize [] (Dimension. 300 300))
    (paint [g] (paint-results-graph g))))

(defn update-results-graph
  []
  (let [data (results-to-dataset)]
    (if (< 1 (nrow data))
      (def *results-graph*
        (let [x-axis (keyword (.getSelectedItem *results-graph-x-dropdown*))
              y-axis (keyword (.getSelectedItem *results-graph-y-dropdown*))]
          (with-data (results-to-dataset)
            (scatter-plot x-axis y-axis :x-label (name x-axis) :y-label (name y-axis)))))
      (def *results-graph* nil)))
  (.repaint *results-graph-panel*))

(defn get-results-tab
  []
  (doto (JPanel. (GridBagLayout.))
    (grid-bag-layout
     :fill :BOTH, :insets (Insets. 5 5 5 5)

     :gridx 0, :gridy 0, :weightx 1.0, :weighty 1.0, :gridwidth 5
     *results-scrollpane*

     :gridx 0, :gridy 1, :weightx 0.0, :weighty 0.0, :gridwidth 1
     (JLabel. "x-axis:")

     :gridx 1
     (doto *results-graph-x-dropdown*
       (. addItemListener (proxy [java.awt.event.ItemListener] []
                            (itemStateChanged [_] (update-results-graph)))))

     :gridx 2
     (JLabel. "y-axis:")

     :gridx 3
     (doto *results-graph-y-dropdown*
       (. addItemListener (proxy [java.awt.event.ItemListener] []
                            (itemStateChanged [_] (update-results-graph)))))

     :gridx 4, :weightx 1.0
     (JPanel.)

     :gridx 0, :gridy 2, :weightx 1.0, :weighty 1.0, :gridwidth 5
     *results-graph-panel*

     :gridx 0, :gridy 3, :weighty 0.0
     *results-checkboxes-panel*)))

(defn update-everything
  [or-state]
  (update-or-state or-state)
  (if (= "A" (:id (:ep-state or-state)))
    (do
      (update-time -1)
      (. *steplabel* (setText "Step: N/A")))
    (do
      (update-time (dec (:time (:ep-state or-state))))
      (. *steplabel* (setText (format "Step: %d" *time*)))))
  
  (update-goto-ep-state-combobox)
  (update-ep-tree-diagram)
  (update-results)
  (update-results-graph)
  (.repaint *problem-diagram*)
  ((:update-stats-fn (:player-fns *problem*)))
  (update-logs))

(defn goto-ep-state-action
  []
  (let [id (re-find #"^[A-Z]+" (. *goto-ep-state-combobox* (getSelectedItem)))
        ep-state-tree (goto-ep-state (:ep-state-tree *or-state*) id)]
    (if-not (root-ep-state? ep-state-tree)
      (update-everything
       (-> *or-state*
           (assoc :ep-state-tree ep-state-tree)
           (assoc :ep-state (current-ep-state ep-state-tree)))))))

(defn step
  []
  (let [or-state (run-simulation-step *problem* *truedata* *or-state* *params*
                                      (. System (nanoTime)))]
    (update-everything
     (assoc or-state :results
            (let [milli (:Milliseconds (last (:results or-state)))
                  prev-milli (:Milliseconds (second (reverse (:results or-state))))]
              (conj (vec (reverse (rest (reverse (:results or-state)))))
                    (assoc (last (:results or-state)) :Milliseconds
                           (if prev-milli
                             (+ milli prev-milli)
                             milli))))))))

(defn new-simulation
  []
  (let [params (get-params)
        meta-abduction (:MetaAbduction params)
        sensors ((:sensor-gen-fn *problem*) params)
        truedata ((:truedata-fn *problem*) params)
        or-state (init-one-run-state meta-abduction sensors
                                     (:initial-problem-data *problem*))]
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
     :gridwidth 1
     
     :gridx 0, :gridy 0
     (JLabel. "MetaAbduction:")
     :gridx 1, :gridy 0
     (:MetaAbduction *param-checkbox*)

     :gridx 0, :gridy 1
     (JLabel. "Steps:")
     :gridx 1, :gridy 1
     (:Steps *param-spinners*)
     
     :gridx 0, :gridy 2
     (JLabel. "SensorReportNoise:")
     :gridx 1, :gridy 2
     (:SensorReportNoise *param-spinners*)
     
     :gridx 0, :gridy 3
     (JLabel. "BeliefNoise:")
     :gridx 1, :gridy 3
     (:BeliefNoise *param-spinners*))))

(def *stats-panel*
  (doto (JPanel. (GridBagLayout.))
    (grid-bag-layout
     :fill :BOTH, :insets (Insets. 5 5 5 5)
     
     :gridx 0, :gridy 0
     *steplabel*)))

(def *logs-tab*
  (doto (JPanel. (GridBagLayout.))
    (grid-bag-layout
     :insets (Insets. 5 5 5 5)
     :weightx 1.0, :fill :BOTH

     :gridx 0, :gridy 0, :weighty 0.0
     (JLabel. "True data log")
     :gridx 0, :gridy 1, :weighty 1.0
     (JScrollPane. *truedata-log-box*)
     
     :gridx 0, :gridy 2, :weighty 0.0
     *abduction-log-label*
     :gridx 0, :gridy 3, :weighty 1.0
     (JScrollPane. *abduction-log-box*)

     :gridx 0, :gridy 4, :weighty 0.0
     *problem-log-label*
     :gridx 0, :gridy 5, :weighty 1.0
     (JScrollPane. *problem-log-box*)

     :gridx 0, :gridy 6, :weighty 0.0
     (JLabel. "Meta-abduction log")
     :gridx 0, :gridy 7, :weighty 1.0
     (JScrollPane. *meta-log-box*))))

(defn get-mainframe
  []
  (doto (JFrame. "Player")
    (.setLayout (GridBagLayout.))
    (grid-bag-layout
     :insets (Insets. 5 5 5 5)
     
     :gridx 0, :gridy 0, :gridheight 7, :fill :BOTH
     :weightx 1.0, :weighty 1.0
     (doto (JTabbedPane.)
       (.addTab "Problem diagram" *problem-diagram*)
       (.addTab "Epistemic state tree" *ep-tree-scrollpane*)
       (.addTab "Logs" *logs-tab*)
       (.addTab "Results" (get-results-tab))
       (.setSelectedIndex 0))
     
     :gridx 1, :gridy 0, :gridheight 1, :gridwidth 2
     :weightx 0.0, :weighty 0.0
     *params-panel*

     :gridx 1, :gridy 1, :insets (Insets. 0 0 0 0)
     *problem-params-panel*

     :gridx 1, :gridy 2, :gridwidth 1, :insets (Insets. 5 5 5 5)
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
  [problem & opts]

  (update-problem problem)
  (update-params (get-params))

  (get-results-graph-x-dropdown)
  (get-results-graph-y-dropdown)

  (def *problem-diagram* ((:get-diagram-fn (:player-fns problem))))
  (def *problem-params-panel* ((:get-params-panel-fn (:player-fns problem))))
  (def *problem-stats-panel* ((:get-stats-panel-fn (:player-fns problem))))
  (def *results-checkboxes* (get-results-checkboxes))
  (def *results-checkboxes-panel* (get-results-checkboxes-panel))

  (def *mainframe* (get-mainframe))

  (let [options (apply hash-map opts)]
    (when (not (:interactive options))
      (doto *mainframe*
        (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)))
    (doto *mainframe*
      (.setResizable true)
      (.pack)
      (.setSize 900 700)
      (.show)))

  (new-simulation))

