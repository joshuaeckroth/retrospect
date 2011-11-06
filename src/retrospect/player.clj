(ns retrospect.player
  (:import (java.awt GridBagLayout Insets Dimension))
  (:import (javax.swing JSpinner SpinnerNumberModel JTabbedPane))
  (:use [clj-swing.frame])
  (:use [clj-swing.label])
  (:use [clj-swing.panel])
  (:use [clj-swing.button])
  (:use [clj-swing.combo-box])
  (:use [clj-swing.text-field])
  (:use [retrospect.problem :only [run-simulation-step]])
  (:use [retrospect.state])
  (:use [retrospect.gui.eptree :only [ep-tree-tab update-ep-tree]])
  (:use [retrospect.gui.explainsgraph :only
         [explains-graph-tab update-explains-graph]])
  (:use [retrospect.gui.results :only [update-results results-tab]])
  (:use [retrospect.gui.logs :only [update-logs logs-tab]])
  (:use [retrospect.workspaces :only [set-last-id]])
  (:use [retrospect.onerun :only [init-one-run-state]])
  (:use [retrospect.epistemicstates :only
         [list-ep-states current-ep-state goto-ep-state root-ep-state?
          previous-ep-state non-accepted-current-ep-state?]])
  (:use [retrospect.random :only [set-seed]]))

(def prepared-selected (atom "None"))
(def params-edit (ref ""))
(def ep-list (ref '[]))
(def ep-selected (atom nil))
(def steplabel (label ""))
(def problem-diagram (panel))

(defn format-params
  [params]
  (str "{\n"
       (apply str (interpose "\n" (for [k (sort (keys params))]
                                    (format "%s %s" k (pr-str (params k))))))
       "\n}"))

(def seed-spinner (JSpinner. (SpinnerNumberModel. 10 nil nil 1)))

(defn get-seed [] (->> seed-spinner .getModel .getNumber .intValue))
(defn set-seed-spinner [seed] (. seed-spinner setValue seed))

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
  #_(update-explains-graph)
  (update-results)
  (update-logs)
  ((:update-stats-fn (:player-fns @problem)))
  ((:update-diagram-fn (:player-fns @problem))))

(defn new-simulation
  []
  (let [prepared? (and (not (nil? @prepared-selected))
                       (not= "None" @prepared-selected))
        ps (read-string @params-edit)]
    (alter-var-root (var params) (constantly ps))
    (set-last-id 0)
    (when (not prepared?)
      (set-seed (get-seed))
      (dosync
       (alter truedata (constantly ((:truedata-fn @problem))))
       (alter sensors (constantly ((:sensor-gen-fn @problem))))))
    (dosync
     (alter or-state (constantly (init-one-run-state
                                  @sensors ((:gen-problem-data-fn @problem)
                                            @truedata @sensors)))))
    (update-everything)))

(defn set-prepared-action
  []
  (when (and (not (nil? @prepared-selected)) (not= "None" @prepared-selected))
    ;; apply the 'prepared' function
    (let [prepared ((get (:prepared-map @problem) @prepared-selected))
          ps (:params prepared)
          seed (if (:seed ps) (:seed ps) 1)
          td (:truedata prepared)
          sens (:sensors prepared)]
      (dosync (alter params-edit (constantly (format-params ps))))
      (alter-var-root (var params) (constantly ps))
      (set-seed seed)
      (set-last-id 0)
      (dosync
       (alter truedata (constantly td))
       (alter sensors (constantly sens))
       (alter or-state (constantly (init-one-run-state
                                    @sensors ((:gen-problem-data-fn @problem)
                                              @truedata @sensors)))))
      (update-everything))))

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
  (let [ors (run-simulation-step @truedata @or-state false true)]
    (dosync (alter or-state (constantly ors)))
    (update-everything)))

(defn next-step
  []
  (when (< @time-now (:Steps params))
    (step)))

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
              ;(.addTab "Explains graph" (explains-graph-tab))
              (.addTab "Results" (results-tab))
              (.setSelectedIndex 0))

          :gridx 1 :gridy 0 :gridheight 1 :gridwidth 2 :weightx 0.0 :weighty 0.0
          _ (combo-box
             [] :model (seq-ref-combobox-model
                        (ref (concat ["None"] (keys (:prepared-map @problem))))
                        prepared-selected))
          :gridx 1 :gridy 1
          _ (button "Set prepared" :action ([_] (set-prepared-action)))

          :gridx 1 :gridy 2 :weighty 0.8
          _ (scroll-panel (text-area :str-ref params-edit :editable true :wrap false))

          :gridx 1 :gridy 3 :gridwidth 1 :weighty 0.0
          _ (label "Seed")
          :gridx 2
          _ seed-spinner

          :gridx 1 :gridy 4
          _ (button "New" :action ([_] (new-simulation)))
          :gridx 2
          _ (button "Next" :action ([_] (next-step)))

          :gridx 1 :gridy 5
          _ (doto (combo-box
                   [] :model (seq-ref-combobox-model ep-list ep-selected))
              (.setMinimumSize (Dimension. 100 0)))
          :gridx 2
          _ (button "Goto" :action ([_] (goto-ep-state-action)))

          :gridx 1 :gridy 6 :gridwidth 2
          _ steplabel

          :gridx 1 :gridy 7
          _ ((:get-stats-panel-fn (:player-fns @problem)))

          :gridy 8 :weighty 1.0
          _ (panel)]))

(defn start-player
  [& opts]

  (alter-var-root (var params) (constantly (:default-params @problem)))
  (dosync (alter params-edit (constantly (format-params params))))

  (let [options (apply hash-map opts)]
    (when (:monitor options)
      (dosync
       (alter or-state (constantly (:or-state options)))
       (alter sensors (constantly (:sensors options)))
       (alter truedata (constantly (:truedata options))))
      (update-everything))
    (when (not (:monitor options))
      (new-simulation))
    ((:setup-diagram-fn (:player-fns @problem)) problem-diagram)
    (mainframe)))
