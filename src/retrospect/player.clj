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
  (:use [retrospect.simulate
         :only [run-simulation-step merge-default-params get-default-params init-ors
                evaluate]])
  (:use [retrospect.state])
  (:use [retrospect.profile :only [profile]])
  (:use [retrospect.gui.eptree :only [ep-tree-tab update-ep-tree]])
  (:use [retrospect.gui.results :only [update-results results-tab]])
  (:use [retrospect.gui.repl :only [update-repl-tab repl-tab]])
  (:use [retrospect.epistemicstates
         :only [list-ep-states cur-ep prev-ep goto-ep ep-path]])
  (:use [geppetto.random :only [rgen new-seed]]))

(def prepared-selected (atom "None"))
(def params-edit (ref ""))
(def ep-list (ref '[]))
(def ep-selected (atom nil))
(def steplabel (label ""))
(def doubt-label (label ""))
(def prefs (atom nil))

(defn get-saved-params
  []
  (let [ps (try (read-string (.get @prefs (format "%s-%s-params"
                                                  (:name @reasoner) (:name @problem))
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
    (.put @prefs (format "%s-%s-params" (:name @reasoner) (:name @problem))
          (pr-str ps))
    (dosync (alter params-edit (constantly (format-params (dissoc ps :Seed)))))))

(defn clear-params
  []
  (dosync (alter params-edit (constantly ""))))

(def seed-spinner (JSpinner. (SpinnerNumberModel. 10 nil nil 1)))
(defn get-seed [] (->> seed-spinner .getModel .getNumber .intValue))
(defn set-seed-spinner [seed] (. seed-spinner setValue seed))

(defn update-everything
  []
  (let [time-n (:time (cur-ep (:est @or-state)))
        time-p (or (first (filter #(not= time-n %)
                             (reverse (sort (map :time (ep-path (:est @or-state)))))))
                   -1)]
    (dosync
     (alter time-now (constantly time-n))
     (alter time-prev (constantly time-p)))
    (. steplabel (setText (format "Step: %d->%d" @time-prev @time-now))))
  (dosync
   (alter ep-list (constantly (sort (list-ep-states (:est @or-state)))))
   (alter results (constantly (:results (cur-ep (:est @or-state))))))
  (let [ws (:workspace (cur-ep (:est @or-state)))]
    (. doubt-label (setText (format "%.2f" (:Doubt (last @results))))))
  (update-ep-tree)
  ((:update-tabs-fn (:player-fns @reasoner)))
  (update-results)
  (update-repl-tab)
  ((:update-stats-fn (:player-fns @problem)))
  (when-let [f (:update-diagram-fn (:player-fns @problem))]
    (f)))

(defn step
  []
  (profile
   (let [ors (run-simulation-step @truedata @or-state true)
         ors-results (evaluate @truedata ors)]
     (dosync (alter or-state (constantly ors-results)))
     (update-everything))))

(defn next-step
  []
  (when (< @time-now (:Steps params))
    (step)))

(def next-button (button "Next" :enabled false :action ([_] (next-step))))

(defn new-simulation
  []
  (.setEnabled next-button true)
  (let [prepared? (and (not (nil? @prepared-selected))
                       (not= "None" @prepared-selected))
        ps (dissoc (merge-default-params (read-string @params-edit)) :simulation)]
    (alter-var-root (var params) (constantly ps))
    (set-last-id 0)
    (when (not prepared?)
      (let [ps (merge-default-params (read-string @params-edit))
            seed (if (:Seed ps) (:Seed ps) (get-seed))
            ps-seed (assoc ps :Seed seed)]
        (alter-var-root (var params) (constantly ps-seed))
        (.put @prefs (format "%s-%s-params" (:name @reasoner) (:name @problem))
              (pr-str ps-seed))
        (dosync (alter params-edit (constantly (format-params (dissoc ps-seed :Seed)))))
        (alter-var-root (var rgen) (constantly (new-seed seed)))
        (set-seed-spinner seed)
        (set-last-id 0)
        (dosync
         (alter truedata (constantly (profile ((:generate-truedata-fn @problem)))))
         (alter sensors (constantly ((:generate-sensors-fn @problem)
                                     (:training @truedata)))))))
    (dosync
     (alter results (constantly []))
     (alter or-state (constantly (init-ors @truedata @sensors))))
    (update-everything)))

(defn set-prepared-action
  []
  (when (and (not (nil? @prepared-selected)) (not= "None" @prepared-selected))
    ;; apply the 'prepared' function
    (let [prepared ((get (:prepared-map @problem) @prepared-selected))
          ps (dissoc (merge-default-params (:params prepared)) :simulation)
          seed (if (:seed ps) (:seed ps) 1)
          ps-seed (assoc ps :Seed seed)
          td (:truedata prepared)
          sens (:sensors prepared)]
      (dosync (alter params-edit (constantly (format-params (dissoc ps-seed :seed)))))
      (alter-var-root (var params) (constantly ps-seed))
      (.put @prefs (format "%s-%s-params" (:name @reasoner) (:name @problem)) (pr-str ps-seed))
      (alter-var-root (var rgen) (constantly (new-seed seed)))
      (set-last-id 0)
      (dosync
       (alter truedata (constantly td))
       (alter sensors (constantly sens))
       (alter results (constantly []))
       (alter or-state (constantly (init-ors @truedata @sensors))))
      (.setEnabled next-button true)
      (update-everything))))

(defn goto-ep-state-action
  []
  (if @ep-selected
    (let [id (re-find #"^[A-Z]+" @ep-selected)]
      (dosync (alter or-state assoc :est (goto-ep (:est @or-state) id)))
      (update-everything))))

(defn mainframe
  []
  (frame :title "retrospect player"
         :layout (GridBagLayout.)
         :constrains (java.awt.GridBagConstraints.)
         :size [1000 700]
         :show true
         :on-close :exit
         [:gridx 0 :gridy 0 :gridheight 12 :weightx 1.0 :weighty 1.0
          :fill :BOTH :insets (Insets. 5 5 5 5)
          _ (let [tabs (JTabbedPane.)]
              (when-let [f (:setup-diagram-fn (:player-fns @problem))]
                (.addTab tabs "Diagram" (f)))
              (doseq [t ((:get-tabs-fn (:player-fns @reasoner)))]
                (.addTab tabs (first t) (second t)))
              (doto tabs
                (.addTab "Ep tree" (ep-tree-tab))
                (.addTab "REPL" (repl-tab))
                (.addTab "Results" (results-tab))
                (.setSelectedIndex 0))
              tabs)

          :gridx 1 :gridy 0 :gridheight 1 :gridwidth 2 :weightx 0.05 :weighty 0.0
          _ (combo-box
             [] :model (seq-ref-combobox-model
                        (ref (concat ["None"] (keys (:prepared-map @problem))))
                        prepared-selected))
          :gridx 1 :gridy 1
          _ (button "Set prepared" :action ([_] (set-prepared-action)))

          :gridx 1 :gridy 2 :weighty 3.0
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
              (.add next-button))

          :gridx 1 :gridy 6
          _ (doto (panel)
              (.add (doto (combo-box
                           [] :model (seq-ref-combobox-model ep-list ep-selected))
                      (.setMinimumSize (Dimension. 100 0))))
              (.add (button "Goto" :action ([_] (goto-ep-state-action)))))

          :gridx 1 :gridy 7
          _ steplabel

          :gridx 1 :gridy 8
          _ (label "Doubt:")
          :gridx 2
          _ doubt-label

          :gridx 1 :gridy 9 :insets (Insets. 0 0 0 0)
          _ ((:get-stats-panel-fn (:player-fns @problem)))

          :gridy 10 :weighty 1.0
          _ (panel)]))

(defn start-player
  [& opts]
  #_(javax.swing.UIManager/setLookAndFeel "org.pushingpixels.substance.api.skin.SubstanceBusinessLookAndFeel")
  (swap! prefs (constantly (.node (Preferences/userRoot) "/cc/artifice/retrospect/player")))
  (get-saved-params)
  (when (:Seed params) (set-seed-spinner (:Seed params)))
  (dosync (alter params-edit (constantly (format-params (dissoc params :Seed)))))

  (let [options (apply hash-map opts)]
    (mainframe)))
