(ns retrospect.player
  (:import (java.awt GridBagLayout Insets Dimension))
  (:import (javax.swing JSpinner SpinnerNumberModel JTabbedPane))
  (:use [clj-swing.frame])
  (:use [clj-swing.label])
  (:use [clj-swing.panel])
  (:use [clj-swing.button])
  (:use [clj-swing.combo-box])
  (:use [retrospect.problem :only [run-simulation-step]])
  (:use [retrospect.state])
  (:use [retrospect.gui.eptree :only [ep-tree-tab update-ep-tree]])
  (:use [retrospect.gui.explainsgraph :only
         [explains-graph-tab update-explains-graph]])
  (:use [retrospect.gui.results :only [update-results results-tab]])
  (:use [retrospect.gui.logs :only [update-logs logs-tab]])
  (:use [retrospect.workspaces :only [last-id]])
  (:use [retrospect.onerun :only [init-one-run-state]])
  (:use [retrospect.epistemicstates :only
         [list-ep-states current-ep-state goto-ep-state root-ep-state?
          previous-ep-state non-accepted-current-ep-state?]]))

(def prepared-selected (atom nil))
(def ep-list (ref '[]))
(def ep-selected (atom nil))
(def steplabel (label ""))
(def problem-diagram (panel))

(def param-spinners
  {:Steps (JSpinner. (SpinnerNumberModel. 50 1 1000 1))
   :StepsBetween (JSpinner. (SpinnerNumberModel. 1 1 1000 1))
   :SensorNoise (JSpinner. (SpinnerNumberModel. 0 0 100 10))
   :BeliefNoise (JSpinner. (SpinnerNumberModel. 0 0 100 10))})

(def param-checkbox
  {:MetaAbduction (check-box :caption "MetaAbduction")
   :Lazy (check-box :caption "Lazy")})

(defn get-params
  []
  (apply hash-map (flatten (concat
                            ((:get-params-fn (:player-fns @problem)))
                            (for [k (keys param-spinners)]
                              [k (->> (k param-spinners)
                                      .getModel .getNumber .intValue)])
                            (for [k (keys param-checkbox)]
                              [k (.isSelected (k param-checkbox))])))))

(defn set-params
  []
  ((:set-params-fn (:player-fns @problem)))
  (doseq [k (keys param-spinners)]
    (. (k param-spinners) setValue (k @params)))
  (. (:MetaAbduction param-checkbox) setSelected (:meta-abduction @or-state))
  (. (:Lazy param-checkbox) setSelected (:lazy @or-state)))

(defn update-everything
  []
  (let [prev-ep (previous-ep-state (:ep-state-tree @or-state))
        time-n (:time (:ep-state @or-state))
        time-p (if prev-ep (:time prev-ep) -1)]
    (dosync
     (alter time-now (constantly time-n))
     (alter time-prev (constantly time-p)))
    (. steplabel (setText (if (nil? prev-ep) "Step: N/A"
                              (format "Step: %d->%d" (dec @time-prev)
                                      (dec @time-now))))))
  (dosync
   (alter ep-list (constantly (sort (list-ep-states (:ep-state-tree @or-state))))))
  (update-ep-tree)
  (update-explains-graph)
  (update-results)
  (update-logs)
  ((:update-stats-fn (:player-fns @problem)))
  ((:update-diagram-fn (:player-fns @problem))))

(defn new-simulation
  []
  (let [prepared? (and (not (nil? @prepared-selected))
                       (not= "None" @prepared-selected))
        ps (get-params)
        meta-abduction (:MetaAbduction ps)
        lazy (:Lazy ps)]
    (when (not prepared?)
      (dosync
       (alter sensors (constantly ((:sensor-gen-fn @problem) ps)))
       (alter truedata (constantly ((:truedata-fn @problem) ps)))))
    (dosync
     (alter params (constantly ps))
     (alter or-state
            (constantly (init-one-run-state meta-abduction lazy @sensors
                                            ((:gen-problem-data-fn @problem)
                                             @sensors ps)))))
    (update-everything)))

(defn set-prepared-action
  []
  (when (and (not (nil? @prepared-selected)) (not= "None" @prepared-selected))
    (let [prepared (get (:prepared-map @problem) @prepared-selected)
          ps (:params prepared)
          meta-abduction (:MetaAbduction ps)
          lazy (:Lazy ps)
          td (:truedata prepared)
          sens (:sensors prepared)]
      (dosync
       (alter params (constantly ps))
       (alter truedata (constantly td))
       (alter sensors (constantly sens))
       (alter or-state
              (constantly (init-one-run-state meta-abduction lazy sens
                                              ((:gen-problem-data-fn @problem)
                                               sens ps)))))
      (update-everything)
      (set-params))))

(defn goto-ep-state-action
  []
  (if @ep-selected
    (let [id (re-find #"^[A-Z]+" @ep-selected)
          est (goto-ep-state (:ep-state-tree @or-state) id)
          ep-state (current-ep-state est)]
      (dosync (alter or-state assoc :ep-state-tree est
                     :ep-state (current-ep-state est)))
      (update-everything))))

(defn step
  []
  (let [ors (run-simulation-step @problem @truedata @or-state @params false true)]
    (dosync (alter or-state (constantly ors)))
    (update-everything)))

(defn next-step
  []
  (when (< @time-now (:Steps @params))
    (step)))

(def generic-params
  (panel :layout (GridBagLayout.)
         :constrains (java.awt.GridBagConstraints.)
         [:gridx 0 :gridy 0 :weightx 1.0 :weighty 0.0
          :fill :BOTH :insets (Insets. 5 5 5 5)
          _ (label "Steps:")
          :gridx 1
          _ (:Steps param-spinners)
          :gridx 0 :gridy 1
          _ (label "StepsBetween:")
          :gridx 1
          _ (:StepsBetween param-spinners)
          :gridx 0 :gridy 2
          _ (label "SensorNoise:")
          :gridx 1
          _ (:SensorNoise param-spinners)
          :gridx 0 :gridy 3
          _ (label "BeliefNoise:")
          :gridx 1
          _ (:BeliefNoise param-spinners)
          :gridy 4 :weighty 1.0
          _ (panel)]))

(defn mainframe
  []
  (frame :title "Player"
         :layout (GridBagLayout.)
         :constrains (java.awt.GridBagConstraints.)
         :size [1000 700]
         :show true
         :on-close :dispose
         [:gridx 0 :gridy 0 :gridheight 9 :weightx 1.0 :weighty 1.0
          :fill :BOTH :insets (Insets. 5 5 5 5)
          _ (doto (JTabbedPane.)
              (.addTab "Problem diagram" problem-diagram)
              (.addTab "Epistemic state tree" (ep-tree-tab))
              (.addTab "Logs" (logs-tab))
              (.addTab "Explains graph" (explains-graph-tab))
              (.addTab "Results" (results-tab))
              (.setSelectedIndex 0))

          :gridx 1 :gridy 0 :gridheight 1 :gridwidth 2 :weightx 0.0 :weighty 0.0
          _ (combo-box
             [] :model (seq-ref-combobox-model
                        (ref (concat ["None"] (keys (:prepared-map @problem))))
                        prepared-selected))
          :gridx 1 :gridy 1
          _ (button "Set prepared" :action ([_] (set-prepared-action)))

          :gridx 1 :gridy 2
          _ (:MetaAbduction param-checkbox)

          :gridx 1 :gridy 3
          _ (:Lazy param-checkbox)

          :gridx 1 :gridy 4 :gridwidth 2 :weighty 1.0
          _ (doto (JTabbedPane.)
              (.setMinimumSize (Dimension. 220 0))
              (.addTab "Generic"
                       (scroll-panel generic-params))
              (.addTab (:name @problem)
                       (scroll-panel
                        ((:get-params-panel-fn (:player-fns @problem))))))
          
          :gridx 1 :gridy 5 :gridwidth 1 :weighty 0.0
          _ (button "New" :action ([_] (new-simulation)))
          :gridx 2
          _ (button "Next" :action ([_] (next-step)))

          :gridx 1 :gridy 6
          _ (doto (combo-box
                   [] :model (seq-ref-combobox-model ep-list ep-selected))
              (.setMinimumSize (Dimension. 100 0)))
          :gridx 2
          _ (button "Goto" :action ([_] (goto-ep-state-action)))

          :gridx 1 :gridy 7 :gridwidth 2
          _ steplabel

          :gridx 1 :gridy 8
          _ ((:get-stats-panel-fn (:player-fns @problem)))]))

(defn start-player
  [prob & opts]

  (dosync
   (alter problem (constantly prob))
   (alter params (constantly (get-params))))

  (let [options (apply hash-map opts)]
    (when (:monitor options)
      (dosync
       (alter params (constantly (:params options)))
       (alter or-state (constantly (:or-state options)))
       (alter sensors (constantly (:sensors options)))
       (alter truedata (constantly (:truedata options))))
      (update-everything)
      (set-params))
    (when (not (:monitor options))
      (new-simulation))
    ((:setup-diagram-fn (:player-fns @problem)) problem-diagram)

    (mainframe)))
