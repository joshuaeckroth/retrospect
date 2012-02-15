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
  (:use [retrospect.problem
         :only [run-simulation-step merge-default-params get-default-params init-ors]])
  (:use [retrospect.state])
  (:use [retrospect.gui.eptree :only [ep-tree-tab update-ep-tree]])
  (:use [retrospect.gui.results :only [update-results results-tab]])
  (:use [retrospect.gui.repl :only [update-repl-tab repl-tab]])
  (:use [retrospect.epistemicstates
         :only [list-ep-states cur-ep prev-ep goto-ep]])
  (:use [retrospect.random :only [rgen new-seed]]))

(def prepared-selected (atom "None"))
(def params-edit (ref ""))
(def ep-list (ref '[]))
(def ep-selected (atom nil))
(def steplabel (label ""))
(def prefs (atom nil))

(defn get-saved-params
  []
  (let [ps (try (read-string (.get @prefs (format "%s-%s-params"
                                                  (:name @reason) (:name @problem))
                                   (pr-str (get-default-params))))
                (catch Exception _ (get-default-params)))]
    (alter-var-root (var params) (constantly ps))))

(defn format-params
  [params]
  (str "{\n"
       (apply str (interpose "\n" (for [k (sort (keys params))]
                                    (format "%s %s" k (pr-str (params k))))))
       "\n}"))

(defn set-default-params
  []
  (let [ps (get-default-params)]
    (alter-var-root (var params) (constantly ps))
    (.put @prefs (format "%s-%s-params" (:name @reason) (:name @problem))
          (pr-str ps))
    (dosync (alter params-edit (constantly (format-params ps))))))

(defn clear-params
  []
  (dosync (alter params-edit (constantly ""))))

(def seed-spinner (JSpinner. (SpinnerNumberModel. 10 nil nil 1)))
(defn get-seed [] (->> seed-spinner .getModel .getNumber .intValue))
(defn set-seed-spinner [seed] (. seed-spinner setValue seed))

(defn update-everything
  []
  (let [time-n (or (:time (cur-ep (:est @or-state))) 0)
        time-p (or (:time (prev-ep (:est @or-state))) 0)]
    (dosync
     (alter time-now (constantly time-n))
     (alter time-prev (constantly time-p)))
    (. steplabel (setText (format "Step: %d->%d" @time-prev
                                  @time-now))))
  (dosync
   (alter ep-list (constantly (sort (list-ep-states (:est @or-state))))))
  (update-ep-tree)
  ((:update-tabs-fn (:player-fns @reason)))
  (update-results)
  (update-repl-tab)
  ((:update-stats-fn (:player-fns @problem)))
  ((:update-diagram-fn (:player-fns @problem))))

(defn new-simulation
  []
  (let [prepared? (and (not (nil? @prepared-selected))
                       (not= "None" @prepared-selected))
        ps (merge-default-params (read-string @params-edit))]
    (alter-var-root (var params) (constantly ps))
    (set-last-id 0)
    (when (not prepared?)
      (let [ps (merge-default-params (read-string @params-edit))]
        (alter-var-root (var params) (constantly ps))
        (.put @prefs (format "%s-%s-params" (:name @reason) (:name @problem)) (pr-str ps))
        (dosync (alter params-edit (constantly (format-params ps))))
        (let [seed (if (:Seed ps) (:Seed ps) (get-seed))]
          (alter-var-root (var rgen) (constantly (new-seed seed))))
        (if (:Seed ps) (set-seed-spinner (:Seed ps)))
        (set-last-id 0)
        (dosync
         (alter truedata (constantly ((:generate-truedata-fn @problem))))
         (alter sensors (constantly ((:generate-sensors-fn @problem)))))))
    (dosync
     (alter or-state (constantly (init-ors @sensors (:training @truedata)))))
    (update-everything)))

(defn set-prepared-action
  []
  (when (and (not (nil? @prepared-selected)) (not= "None" @prepared-selected))
    ;; apply the 'prepared' function
    (let [prepared ((get (:prepared-map @problem) @prepared-selected))
          ps (merge-default-params (:params prepared))
          seed (if (:seed ps) (:seed ps) 1)
          td (:truedata prepared)
          sens (:sensors prepared)]
      (dosync (alter params-edit (constantly (format-params ps))))
      (alter-var-root (var params) (constantly ps))
      (.put @prefs (format "%s-%s-params" (:name @reason) (:name @problem)) (pr-str ps))
      (alter-var-root (var rgen) (constantly (new-seed seed)))
      (set-last-id 0)
      (dosync
       (alter truedata (constantly td))
       (alter sensors (constantly sens))
       (alter or-state (constantly (init-ors @sensors (:training @truedata)))))
      (update-everything))))

(defn goto-ep-state-action
  []
  (if @ep-selected
    (let [id (re-find #"^[A-Z]+" @ep-selected)]
      (dosync (alter or-state assoc :est (goto-ep (:est @or-state) id)))
      (update-everything))))

(defn step
  []
  (let [ors (run-simulation-step @truedata @or-state true)]
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
         :size [800 600]
         :show true
         :on-close :exit
         [:gridx 0 :gridy 0 :gridheight 10 :weightx 1.0 :weighty 1.0
          :fill :BOTH :insets (Insets. 5 5 5 5)
          _ (let [tabs (doto (JTabbedPane.)
                         (.addTab "Diagram" ((:setup-diagram-fn (:player-fns @problem))))
                         (.addTab "Ep tree" (ep-tree-tab)))]
              (doseq [t ((:get-tabs-fn (:player-fns @reason)))]
                (.addTab tabs (first t) (second t)))
              (doto tabs
                (.addTab "REPL" (repl-tab))
                (.addTab "Results" (results-tab))
                (.setSelectedIndex 0))
              tabs)

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
    (new-simulation)
    (mainframe)))
