(ns retrospect.player
  (:import (java.awt GridBagLayout Insets Dimension))
  (:import (javax.swing JSpinner SpinnerNumberModel JTabbedPane))
  (:import (java.util.prefs Preferences))
  (:use [clj-swing.frame])
  (:use [clj-swing.label])
  (:use [clj-swing.panel])
  (:use [clj-swing.button])
  (:use [clj-swing.combo-box])
  (:use [clj-swing.text-field])
  (:use [retrospect.problem :only [run-simulation-step]])
  (:use [retrospect.state])
  (:use [retrospect.gui.eptree :only [ep-tree-tab update-ep-tree]])
  (:use [retrospect.gui.depgraph :only [depgraph-tab update-depgraph]])
  (:use [retrospect.gui.hypgraph :only [hypgraph-tab update-hypgraph]])
  (:use [retrospect.gui.explainsgraph :only
         [explains-graph-tab update-explains-graph]])
  (:use [retrospect.gui.results :only [update-results results-tab]])
  (:use [retrospect.gui.logs :only [update-logs logs-tab]])
  (:use [retrospect.gui.repl :only [update-repl-tab repl-tab]])
  (:use [retrospect.workspaces :only [set-last-id]])
  (:use [retrospect.onerun :only [init-one-run-state]])
  (:use [retrospect.epistemicstates :only
         [list-ep-states current-ep-state goto-ep-state root-ep-state?
          previous-ep-state non-accepted-current-ep-state?
          nth-previous-ep-state]])
  (:use [retrospect.random :only [rgen new-seed]]))

(def prepared-selected (atom "None"))
(def params-edit (ref ""))
(def ep-list (ref '[]))
(def ep-selected (atom nil))
(def steplabel (label ""))
(def prefs (atom nil))

(defn get-saved-params
  []
  (let [ps (try (read-string (.get @prefs "params"
                                   (pr-str (:default-params @problem))))
                (catch Exception _ (:default-params @problem)))]
    (alter-var-root (var params) (constantly ps))))

(defn format-params
  [params]
  (str "{\n"
       (apply str (interpose "\n" (for [k (sort (keys params))]
                                    (format "%s %s" k (pr-str (params k))))))
       "\n}"))

(defn set-default-params
  []
  (alter-var-root (var params) (:default-params @problem))
  (.put @prefs "params" (pr-str (:default-params @problem)))
  (dosync (alter params-edit (constantly (format-params (:default-params @problem))))))

(defn clear-params
  []
  (dosync (alter params-edit (constantly ""))))

(def seed-spinner (JSpinner. (SpinnerNumberModel. 10 nil nil 1)))

(defn get-seed [] (->> seed-spinner .getModel .getNumber .intValue))
(defn set-seed-spinner [seed] (. seed-spinner setValue seed))

(defn update-everything
  []
  (let [est (:ep-state-tree @or-state)
        ep-state (let [ep (current-ep-state est)]
                   (if (re-find #"\?" (str ep))
                     (previous-ep-state est) ep))
        prev-ep (let [ep (current-ep-state est)]
                  (if (re-find #"\?" (str ep))
                    (nth-previous-ep-state est 2)
                    (previous-ep-state est)))
        time-n (or (:time ep-state) 0)
        time-p (or (:time prev-ep) 0)]
    (dosync
     (alter time-now (constantly time-n))
     (alter time-prev (constantly time-p)))
    (. steplabel (setText (format "Step: %d->%d" @time-prev
                                  @time-now))))
  (dosync
   (alter ep-list (constantly (sort (list-ep-states (:ep-state-tree @or-state))))))
  (update-ep-tree)
  (update-depgraph)
  (update-hypgraph)
  #_(update-explains-graph)
  (update-results)
  (update-logs)
  (update-repl-tab)
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
      (let [ps (read-string @params-edit)]
        (alter-var-root (var params) (constantly ps))
        (.put @prefs "params" (pr-str ps))
        (dosync (alter params-edit (constantly (format-params ps))))
        (let [seed (if (:Seed ps) (:Seed ps) (get-seed))]
          (alter-var-root (var rgen) (constantly (new-seed seed))))
        (if (:Seed ps) (set-seed-spinner (:Seed ps)))
        (set-last-id 0)
        (dosync
         (alter truedata (constantly ((:truedata-fn @problem))))
         (alter sensors (constantly ((:sensor-gen-fn @problem)))))))
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
      (.put @prefs "params" (pr-str ps))
      (alter-var-root (var rgen) (constantly (new-seed seed)))
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
         :on-close :exit
         [:gridx 0 :gridy 0 :gridheight 10 :weightx 1.0 :weighty 1.0
          :fill :BOTH :insets (Insets. 5 5 5 5)
          _ (doto (JTabbedPane.)
              (.addTab "Diagram" ((:setup-diagram-fn (:player-fns @problem))))
              (.addTab "Ep tree" (ep-tree-tab))
              (.addTab "Logs" (logs-tab))
              (.addTab "Hyp graph" (hypgraph-tab))
              (.addTab "Dep graph" (depgraph-tab))
              ;;(.addTab "Explains graph" (explains-graph-tab))
              (.addTab "REPL" (repl-tab))
              (.addTab "Results" (results-tab))
              (.setSelectedIndex 0))

          :gridx 1 :gridy 0 :gridheight 1 :gridwidth 2 :weightx 0.0 :weighty 0.0
          _ (combo-box
             [] :model (seq-ref-combobox-model
                        (ref (concat ["None"] (keys (:prepared-map @problem))))
                        prepared-selected))
          :gridx 1 :gridy 1
          _ (button "Set prepared" :action ([_] (set-prepared-action)))

          :gridx 1 :gridy 2 :weighty 1.75
          _ (scroll-panel (text-area :str-ref params-edit :editable true
                                     :wrap false :rows 30))

          :gridx 1 :gridy 3 :gridwidth 1 :weighty 0.0
          _ (button "Default params" :action ([_] (set-default-params)))

          :gridx 2
          _ (button "Clear" :action ([_] (clear-params)))

          :gridx 1 :gridy 4 :gridwidth 1 :weighty 0.0
          _ (label "Seed")
          :gridx 2
          _ seed-spinner

          :gridx 1 :gridy 5 :gridwidth 2
          _ (doto (panel)
              (.add (button "New" :action ([_] (new-simulation))))
              (.add (button "Next" :action ([_] (next-step)))))

          :gridx 1 :gridy 6
          _ (doto (panel)
              (.add (doto (combo-box
                           [] :model (seq-ref-combobox-model ep-list ep-selected))
                      (.setMinimumSize (Dimension. 100 0))))
              (.add (button "Goto" :action ([_] (goto-ep-state-action)))))

          :gridx 1 :gridy 7
          _ steplabel

          :gridx 1 :gridy 8
          _ ((:get-stats-panel-fn (:player-fns @problem)))

          :gridy 9 :weighty 1.0
          _ (panel)]))

(defn start-player
  [& opts]

  (swap! prefs (constantly (.node (Preferences/userRoot) "/cc/artifice/retrospect/player")))
  (get-saved-params)
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
    (mainframe)))
