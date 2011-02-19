(ns samre.player.gui
  (:import [misc WrapLayout])
  (:import (java.awt Color GridBagLayout FlowLayout Insets Dimension Checkbox))
  (:import (java.awt.image BufferedImage))
  (:import (javax.swing JPanel JFrame JButton JTextField JTextArea
			JLabel JScrollPane JSpinner SpinnerNumberModel JComboBox
                        ImageIcon JViewport Scrollable JTabbedPane JTable))
  (:import (java.util Vector))
  (:use [clojure.java.shell :only [sh]])
  (:use [incanter.core :only [to-list with-data dataset nrow]])
  (:use [incanter.charts :only [scatter-plot]])
  (:use [samre.problem :only [get-headers run-simulation-step]])
  (:use [samre.player.state])
  (:use [samre.onerun :only [init-one-run-state]])
  (:use [samre.epistemicstates :only
         [draw-ep-state-tree list-ep-states current-ep-state goto-ep-state
          goto-next-ep-state previous-ep-state]]))

(def *mainframe* nil)
(def *problem-headers* nil)
(def *problem-diagram* nil)
(def *problem-params-panel* nil)
(def *problem-stats-panel* nil)

(def *prepared-list* (JComboBox.))

(def *steplabel* (JLabel. "Step: "))
(def *goto-ep-state-combobox* (JComboBox. (to-array "")))

(def *ep-tree* (BufferedImage. 1 1 (. BufferedImage TYPE_4BYTE_ABGR)))
(def *explains-graph-index* 0)
(def *explains-graph-count* 0)
(def *explains-graph* nil)
(def *prev-explains-graph-button* nil)
(def *next-explains-graph-button* nil)
(def *explains-graph-label* (JLabel.))
(def *truedata-log-box* (JTextArea.))
(def *abduction-log-label* (JLabel. "Abduction log"))
(def *abduction-log-box* (JTextArea.))
(def *hyp-choice* (JComboBox.))
(def *hyp-box* (JTextArea.))
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
   :StepsBetween (JSpinner. (SpinnerNumberModel. 1 1 1000 1))
   :SensorReportNoise (JSpinner. (SpinnerNumberModel. 0 0 100 10))
   :BeliefNoise (JSpinner. (SpinnerNumberModel. 0 0 100 10))})

(def *param-checkbox*
  {:MetaAbduction (Checkbox.)
   :Lazy (Checkbox.)})

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

(defn set-params
  []
  ((:set-params-fn (:player-fns *problem*)))
  (doseq [k (keys *param-spinners*)]
    (. (k *param-spinners*) setValue (k *params*)))
  (. (:MetaAbduction *param-checkbox*) setState (:meta-abduction *or-state*))
  (. (:Lazy *param-checkbox*) setState (:lazy *or-state*)))

(defn get-prepared-list
  []
  (let [prepared-map (:prepared-map *problem*)]
    (.addItem *prepared-list* "None")
    (doseq [p (keys prepared-map)]
      (.addItem *prepared-list* p))))

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
  (. *ep-tree-scrollpane* setViewport (get-ep-tree-viewport)))

(defn draw-explains-graph
  []
  (if-let [prev-ep (previous-ep-state (:ep-state-tree *or-state*))]
    (let [dot (:dot (:workspace prev-ep))
          filename (str "explains-graphs/" *explains-graph-index*)]
      (. *explains-graph-label* setText
         (format "Cycle %d of %d" (inc *explains-graph-index*) *explains-graph-count*))
      (spit (str filename ".dot") (nth dot *explains-graph-index*))
      (sh "dot" "-Tpng" (str "-o" filename ".png") (str filename ".dot"))
      (sh "mogrify" "-resize" "50%" (str filename ".png"))
      (str filename ".png"))))

(defn get-explains-graph-viewport
  []
  (if *explains-graph*
    (let [imageicon (ImageIcon. *explains-graph*)]
      (.. imageicon getImage flush)
      (doto (JViewport.)
        (.setBackground Color/white)
        (.setView (JLabel. imageicon))))
    (doto (JViewport.) (.setBackground Color/white))))

(def *explains-graph-scrollpane*
  (JScrollPane. (get-explains-graph-viewport)))

(defn prev-explains-graph
  []
  (when (> *explains-graph-index* 0)
    (def *explains-graph-index* (dec *explains-graph-index*))
    (when (= 0 *explains-graph-index*)
      (. *prev-explains-graph-button* setEnabled false))
    (. *next-explains-graph-button* setEnabled true)
    (def *explains-graph* (draw-explains-graph))
    (. *explains-graph-scrollpane* setViewport (get-explains-graph-viewport))))

(defn next-explains-graph
  []
  (when (< *explains-graph-index* (dec *explains-graph-count*))
    (def *explains-graph-index* (inc *explains-graph-index*))
    (. *prev-explains-graph-button* setEnabled true)
    (when (= (dec *explains-graph-count*) *explains-graph-index*)
      (. *next-explains-graph-button* setEnabled false))
    (def *explains-graph* (draw-explains-graph))
    (. *explains-graph-scrollpane* setViewport (get-explains-graph-viewport))))

(defn update-explains-graph-diagram
  []
  (if-let [prev-ep (previous-ep-state (:ep-state-tree *or-state*))]
    (do
      (def *explains-graph-index* 0)
      (def *explains-graph-count* (count (:dot (:workspace prev-ep))))
      (def *explains-graph* (draw-explains-graph))
      (. *explains-graph-scrollpane* setViewport (get-explains-graph-viewport))
      (. *prev-explains-graph-button* setEnabled false)
      (if (< 1 *explains-graph-count*)
        (. *next-explains-graph-button* setEnabled true)
        (. *next-explains-graph-button* setEnabled false)))
    (do
      (def *explains-graph-index* 0)
      (def *explains-graph-count* 0)
      (def *explains-graph* nil)
      (. *prev-explains-graph-button* setEnabled false)
      (. *next-explains-graph-button* setEnabled false)
      (. *explains-graph-scrollpane* setViewport (get-explains-graph-viewport))
      (. *explains-graph-label* setText ""))))

(defn get-explains-graph-tab
  []
  (doto (JPanel. (GridBagLayout.))
    (grid-bag-layout
     :fill :BOTH, :insets (Insets. 5 5 5 5)

     :gridx 0, :gridy 0, :weightx 1.0, :weighty 1.0, :gridwidth 6
     *explains-graph-scrollpane*

     :gridx 0, :gridy 1, :weightx 0.0, :weighty 0.0, :gridwidth 1
     *prev-explains-graph-button*

     :gridx 1
     *next-explains-graph-button*

     :gridx 2, :weightx 1.0
     *explains-graph-label*

     :gridx 3, :weightx 0.0
     (doto (JLabel. " unexplained ") (.setForeground Color/orange))
     
     :gridx 4, :weightx 0.0
     (doto (JLabel. " accepted ") (.setForeground Color/blue))

     :gridx 5, :weightx 0.0
     (doto (JLabel. " rejected ") (.setForeground Color/red)))))

(defn update-goto-ep-state-combobox
  []
  (loop [cb (doto *goto-ep-state-combobox* (.removeAllItems))
         ep-states (sort (list-ep-states (:ep-state-tree *or-state*)))]
    (if (empty? ep-states)
      (def *goto-ep-state-combobox*
        (doto cb (.setSelectedItem (str (previous-ep-state (:ep-state-tree *or-state*))))))
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
       (apply str (interpose "\n---\n"
                             (map (fn [ls] (apply str (interpose "\n" (map str ls))))
                                            (:meta-log *or-state*)))))))

(defn update-hyp-box
  []
  (if-let [ep (previous-ep-state (:ep-state-tree *or-state*))]
    (if-let [hyp (first (filter #(= (:pid %) (.getSelectedItem *hyp-choice*))
                                (vals (:hyps (:workspace ep)))))]
      (. *hyp-box* setText
         (apply str ((:str-fn hyp) hyp) "\n\n"
                (interpose
                 "\n" (map str (filter (fn [l] (some #(= % (:id hyp)) (:hyp-ids l)))
                                       (:abducer-log (:workspace ep))))))))
    (. *hyp-box* setText "")))

(defn update-hyp-choice-dropdown
  []
  (.removeAllItems *hyp-choice*)
  (when-let [ep (previous-ep-state (:ep-state-tree *or-state*))]
    (doseq [i (sort (map :pid (vals (:hyps (:workspace ep)))))]
      (.addItem *hyp-choice* i))))

(defn get-results-viewport
  []
  (let [headers (filter (fn [h] (if-let [cb (get *results-checkboxes* h)]
                                  (.getState cb)))
                        *problem-headers*)
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
  (reduce (fn [m h] (let [cb (Checkbox. (name h) false)]
                      (. cb addItemListener
                         (proxy [java.awt.event.ItemListener] []
                           (itemStateChanged [_] (update-results))))
                      (assoc m h cb)))
          {} *problem-headers*))

(defn get-results-checkboxes-panel
  []
  (loop [p (JPanel. (WrapLayout. FlowLayout/LEFT))
         cbs *problem-headers*]
    (if (empty? cbs) p
        (recur (doto p (.add (get *results-checkboxes* (first cbs)))) (rest cbs)))))

(defn results-to-dataset
  []
  (dataset *problem-headers*
           (map (fn [r] (map (fn [h] (get r h))
                             *problem-headers*))
                (:results *or-state*))))

(def *results-graph-x-dropdown* (JComboBox.))

(defn get-results-graph-x-dropdown
  []
  (def *results-graph-x-dropdown*
    (JComboBox. (to-array (map name *problem-headers*)))))

(def *results-graph-y-dropdown* (JComboBox.))

(defn get-results-graph-y-dropdown
  []
  (def *results-graph-y-dropdown*
    (JComboBox. (to-array (map name *problem-headers*)))))

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
    (let [prev-ep (previous-ep-state (:ep-state-tree *or-state*))
          time-now (dec (:time (:ep-state or-state)))
          time-prev (dec (:time prev-ep))]
      (update-time time-now)
      (update-time-prev time-prev)
      (. *steplabel* (setText (if time-prev (format "Step: %d->%d" time-prev time-now)
                                  (format "Step: N/A->%d" time-now))))))
  (update-goto-ep-state-combobox)
  (update-ep-tree-diagram)
  (update-explains-graph-diagram)
  (update-hyp-choice-dropdown)
  (update-hyp-box)
  (update-results)
  (update-results-graph)
  (.repaint *problem-diagram*)
  ((:update-stats-fn (:player-fns *problem*)))
  (update-logs))

(defn goto-ep-state-action
  []
  (let [id (re-find #"^[A-Z]+" (. *goto-ep-state-combobox* (getSelectedItem)))
        ep-state-tree (goto-next-ep-state (goto-ep-state (:ep-state-tree *or-state*) id))]
    (update-everything
     (-> *or-state*
         (assoc :ep-state-tree ep-state-tree)
         (assoc :ep-state (current-ep-state ep-state-tree))))))

(defn step
  []
  (let [or-state (run-simulation-step *problem* *truedata* *or-state*
                                      *params* false true)]
    (update-everything or-state)))

(defn new-simulation
  []
  (let [prepared-choice (.getSelectedItem *prepared-list*)
        prepared (if (not= "None" prepared-choice) (get (:prepared-map *problem*)
                                                        prepared-choice))
        params (if prepared (:params prepared) (get-params))
        meta-abduction (:MetaAbduction params)
        lazy (:Lazy params)
        sensors (if prepared (:sensors prepared) ((:sensor-gen-fn *problem*) params))
        truedata (if prepared (:truedata prepared) ((:truedata-fn *problem*) params))
        or-state (init-one-run-state meta-abduction lazy sensors
                                     ((:gen-problem-data-fn *problem*) sensors params))]
    (update-params params)
    (update-sensors sensors)
    (update-truedata truedata)
    (update-everything or-state)
    (when prepared-choice (set-params))))

(def *newbutton*
  (let [b (JButton. "New")]
    (with-action b e (new-simulation))
    b))

(defn next-step
  []
  (when (< *time* (- (:Steps *params*) (:StepsBetween *params*)))
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
     (JLabel. "Prepared:")
     :gridx 1, :gridy 0
     *prepared-list*
     
     :gridx 0, :gridy 1
     (JLabel. "MetaAbduction:")
     :gridx 1, :gridy 1
     (:MetaAbduction *param-checkbox*)

     :gridx 0, :gridy 2
     (JLabel. "Lazy:")
     :gridx 1, :gridy 2
     (:Lazy *param-checkbox*)

     :gridx 0, :gridy 3
     (JLabel. "Steps:")
     :gridx 1, :gridy 3
     (:Steps *param-spinners*)

     :gridx 0, :gridy 4
     (JLabel. "StepsBetween:")
     :gridx 1, :gridy 4
     (:StepsBetween *param-spinners*)
     
     :gridx 0, :gridy 5
     (JLabel. "SensorReportNoise:")
     :gridx 1, :gridy 5
     (:SensorReportNoise *param-spinners*)
     
     :gridx 0, :gridy 6
     (JLabel. "BeliefNoise:")
     :gridx 1, :gridy 6
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
     (doto *hyp-choice*
       (. addItemListener (proxy [java.awt.event.ItemListener] []
                            (itemStateChanged [_] (update-hyp-box)))))
     :gridx 0, :gridy 5, :weighty 1.0
     (JScrollPane. *hyp-box*)

     :gridx 0, :gridy 6, :weighty 0.0
     *problem-log-label*
     :gridx 0, :gridy 7, :weighty 1.0
     (JScrollPane. *problem-log-box*)

     :gridx 0, :gridy 8, :weighty 0.0
     (JLabel. "Meta-abduction log")
     :gridx 0, :gridy 9, :weighty 1.0
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
       (.addTab "Explains graph" (get-explains-graph-tab))
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
  (def *problem-headers* (get-headers *problem*))

  (update-params (get-params))

  (get-results-graph-x-dropdown)
  (get-results-graph-y-dropdown)

  (def *prev-explains-graph-button*
    (let [b (JButton. "Prev")]
      (with-action b e (prev-explains-graph))
      (doto b (.setEnabled false))))
  
  (def *next-explains-graph-button*
    (let [b (JButton. "Next")]
      (with-action b e (next-explains-graph))
      (doto b (.setEnabled false))))

  (def *problem-diagram* ((:get-diagram-fn (:player-fns problem))))
  (def *problem-params-panel* ((:get-params-panel-fn (:player-fns problem))))
  (def *problem-stats-panel* ((:get-stats-panel-fn (:player-fns problem))))
  (def *results-checkboxes* (get-results-checkboxes))
  (def *results-checkboxes-panel* (get-results-checkboxes-panel))

  (get-prepared-list)

  (def *mainframe* (get-mainframe))

  (let [options (apply hash-map opts)]
    (when (:monitor options)
      (update-params (:params options))
      (update-sensors (:sensors options))
      (update-truedata (:truedata options))
      (update-everything (:or-state options))
      (set-params))
    (when (not (:interactive options))
      (doto *mainframe*
        (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)))
    (doto *mainframe*
      (.setResizable true)
      (.pack)
      (.setSize 900 750)
      (.show))
    (when (not (:monitor options))
      (new-simulation))))

