(ns retrospect.player
  (:import (java.awt GridBagLayout Insets Dimension))
  (:import (javax.swing JSpinner SpinnerNumberModel JTabbedPane UIManager))
  (:use [clj-swing.frame])
  (:use [clj-swing.label])
  (:use [clj-swing.panel])
  (:use [clj-swing.button])
  (:use [clj-swing.combo-box])
  (:use [retrospect.problem :only [run-simulation-step]])
  (:use [retrospect.state])
  (:use [retrospect.gui.eptree :only [ep-tree-tab update-ep-tree]])
  (:use [retrospect.gui.explainsgraph :only [explains-graph-tab update-explains-graph]])
  (:use [retrospect.gui.results :only [update-results update-results-graph results-tab]])
  (:use [retrospect.gui.logs :only [update-logs logs-tab]])
  (:use [retrospect.onerun :only [init-one-run-state]])
  (:use [retrospect.workspaces :only [last-id]])
  (:use [retrospect.epistemicstates :only
         [list-ep-states current-ep-state goto-ep-state root-ep-state?
          goto-next-ep-state previous-ep-state non-accepted-current-ep-state?]]))

(def prepared-selected (atom nil))
(def ep-list (ref '[]))
(def ep-selected (atom nil))
(def steplabel (label ""))
(def problem-diagram (panel))

(def param-spinners
  {:Steps (JSpinner. (SpinnerNumberModel. 50 1 1000 1))
   :StepsBetween (JSpinner. (SpinnerNumberModel. 1 1 1000 1))
   :SensorReportNoise (JSpinner. (SpinnerNumberModel. 0 0 100 10))
   :BeliefNoise (JSpinner. (SpinnerNumberModel. 0 0 100 10))})

(def param-checkbox
  {:MetaAbduction (check-box)
   :Lazy (check-box)})

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
  (. (:MetaAbduction param-checkbox) setState (:meta-abduction @or-state))
  (. (:Lazy param-checkbox) setState (:lazy @or-state)))

(defn update-everything
  []
  (if (root-ep-state? (:ep-state @or-state))
    (do
      (dosync
       (alter time-now (constantly -1))
       (alter time-prev (constantly -1)))
      (. steplabel (setText "Step: N/A")))
    (let [prev-ep (previous-ep-state (:ep-state-tree @or-state))
          time-n (:time (:ep-state @or-state))
          time-p (if prev-ep (:time prev-ep) -1)]
      (dosync
       (alter time-now (constantly time-n))
       (alter time-prev (constantly time-p)))
      (. steplabel (setText (if time-prev
                              (format "Step: %d->%d" @time-prev @time-now)
                              (format "Step: N/A->%d" @time-now))))))
  (dosync
   (alter ep-list (constantly (sort (list-ep-states (:ep-state-tree @or-state))))))
  (update-ep-tree)
  (update-explains-graph)
  (update-results)
  (update-results-graph)
  (update-logs)
  ((:update-stats-fn (:player-fns @problem)))
  ((:update-diagram-fn (:player-fns @problem))))

(defn new-simulation
  []
  (let [prepared (if (not= "None" @prepared-selected)
                   (get (:prepared-map @problem) @prepared-selected))
        ps (if prepared (:params prepared) (get-params))
        meta-abduction (:MetaAbduction ps)
        lazy (:Lazy ps)
        sens (if prepared (:sensors prepared) ((:sensor-gen-fn @problem) ps))
        td (if prepared (:truedata prepared) ((:truedata-fn @problem) ps))
        ors (init-one-run-state meta-abduction lazy sens
                                ((:gen-problem-data-fn @problem) sens ps))]
    (dosync
     (alter last-id (constantly 0))
     (alter params (constantly ps))
     (alter or-state (constantly ors))
     (alter sensors (constantly sens))
     (alter truedata (constantly td)))
    (update-everything)
    (when @prepared-selected (set-params ps))))

(defn goto-ep-state-action
  []
  (if @ep-selected
    (let [id (re-find #"^[A-Z]+" @ep-selected)
          est (goto-ep-state (:ep-state-tree @or-state) id)
          ep-state (current-ep-state est)
          est2 (if (non-accepted-current-ep-state? est) est
                   (goto-next-ep-state est))]
      (dosync (alter or-state assoc :ep-state-tree est2 :ep-state (current-ep-state est2)))
      (update-everything))))

(defn step
  []
  (let [ors (run-simulation-step @problem @truedata @or-state @params false true)]
    (dosync (alter or-state (constantly ors)))
    (update-everything)))

(defn next-step
  []
  (when (< @time-now (- (:Steps @params) (:StepsBetween @params)))
    (step)))

(def generic-params
  (panel :layout (GridBagLayout.)
         :constrains (java.awt.GridBagConstraints.)
         [:gridx 0 :gridy 0 :fill :BOTH :insets (Insets. 5 5 5 5)
          _ (label "Steps:")
          :gridx 1
          _ (:Steps param-spinners)
          :gridx 0 :gridy 1
          _ (label "StepsBetween:")
          :gridx 1
          _ (:StepsBetween param-spinners)
          :gridx 0 :gridy 2
          _ (label "SensorReportNoise:")
          :gridx 1
          _ (:SensorReportNoise param-spinners)
          :gridx 0 :gridy 3
          _ (label "BeliefNoise:")
          :gridx 1
          _ (:BeliefNoise param-spinners)]))

(defn mainframe
  []
  (frame :title "Player"
         :layout (GridBagLayout.)
         :constrains (java.awt.GridBagConstraints.)
         :show true :size [800 600] :on-close :exit
         [:gridx 0 :gridy 0 :gridheight 8 :weightx 1.0 :weighty 1.0
          :fill :BOTH :insets (Insets. 5 5 5 5)
          _ (doto (JTabbedPane.)
              (.addTab "Problem diagram" problem-diagram)
              (.addTab "Epistemic state tree" (ep-tree-tab))
              (.addTab "Logs" (logs-tab))
              (.addTab "Explains graph" (explains-graph-tab))
              (.addTab "Results" (results-tab))
              (.setSelectedIndex 0))

          :gridx 1 :gridy 0 :gridheight 1 :weightx 0.0 :weighty 0.0
          _ (label "Prepared:")
          :gridx 2
          _ (combo-box
             [] :model (seq-ref-combobox-model
                        (ref (concat ["None"] (keys (:prepared-map @problem))))
                        prepared-selected))

          :gridx 1 :gridy 1
          _ (label "MetaAbduction:")
          :gridx 2
          _ (:MetaAbduction param-checkbox)

          :gridx 1 :gridy 2
          _ (label "Lazy:")
          :gridx 2
          _ (:Lazy param-checkbox)

          :gridx 1 :gridy 3 :gridwidth 2 :weighty 1.0
          _ (doto (JTabbedPane.)
              (.addTab "Generic"
                       (scroll-panel generic-params))
              (.addTab (:name @problem)
                       (scroll-panel ((:get-params-panel-fn (:player-fns @problem))))))
          
          :gridx 1 :gridy 4 :gridwidth 1 :weighty 0.0
          _ (button "New" :action ([_] (new-simulation)))
          :gridx 2 :gridy 4
          _ (button "Next" :action ([_] (next-step)))

          :gridx 1 :gridy 5
          _ (combo-box [] :model (seq-ref-combobox-model ep-list ep-selected))
          :gridx 2 :gridy 5
          _ (button "Goto" :action ([_] (goto-ep-state-action)))

          :gridx 1 :gridy 6 :gridwidth 2
          _ steplabel

          :gridx 1 :gridy 7
          _ ((:get-stats-panel-fn (:player-fns @problem)))]))

(defn start-player
  [prob & opts]

  (. UIManager setLookAndFeel (. UIManager getSystemLookAndFeelClassName))

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
      (set-params (:params options)))
    (when (not (:monitor options))
      (new-simulation)))
  ((:setup-diagram-fn (:player-fns @problem)) problem-diagram)

  (mainframe))