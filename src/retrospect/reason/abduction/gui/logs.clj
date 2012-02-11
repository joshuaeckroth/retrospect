(ns retrospect.reason.abduction.gui.logs
  (:import (java.awt GridBagLayout Insets Dimension Font))
  (:import (javax.swing Box JScrollBar))
  (:import (misc AlphanumComparator))
  (:use [clj-swing.label])
  (:use [clj-swing.text-field])
  (:use [clj-swing.tree])
  (:use [clj-swing.button])
  (:use [clj-swing.panel])
  (:use [clojure.contrib.seq :only [find-first]])
  (:require [clojure.set :as set])
  (:require [retrospect.reason.abduction.workspace :as ws])
  (:use [retrospect.epistemicstates :only
         [cur-ep flatten-est]])
  #_(:use [retrospect.reason.abduction.robustness :only [analyze-dependency]])
  (:use [retrospect.confidences])
  (:use [retrospect.state]))

(def truedata-log (ref ""))
(def truedata-log-textbox (ref nil))
(def problem-log (ref ""))
(def problem-log-textbox (ref nil))
(def problem-log-label (label ""))
(def hyp-choices (ref []))
(def hyp-selected (atom nil))
(def workspace-selected (atom nil))
(def hyp-info (ref ""))
(def abduction-tree-map (ref {}))
(def workspace-log-textbox (ref nil))
(def workspace-log (ref ""))

(defn build-abduction-tree-map
  [or-state]
  (let [est (:est or-state)
        list-hyps #(apply sorted-map-by (AlphanumComparator.)
                          (mapcat (fn [h] [(:id h) nil]) %))
        ep-states (flatten-est est)
        ws-fn (fn [wslog]
                {"Added" (list-hyps (map :hyp (:added wslog)))
                 "Forced" (list-hyps (:forced wslog))
                 "Cycles" (apply sorted-map-by (AlphanumComparator.)
                                 (mapcat (fn [i]
                                           (let [b (nth (:best wslog) i)
                                                 ;; seq of {:acc :rej} pairs (maps)
                                                 ars (get (:accrej wslog) (inc i))]
                                             [(format "Cycle %d (%s)%s" (inc i)
                                                      (if (:essential? b)
                                                        "essential"
                                                        (format "delta %.2f"
                                                                (:delta b)))
                                                      (if (:transitive? b)
                                                        " (trans)" ""))
                                              {"Best"
                                               {(:id (:best b)) nil}
                                               "Explained"
                                               {(:id (:explained b)) nil}
                                               "Accepted"
                                               (list-hyps (disj (set (map :acc ars))
                                                                (:best b)))
                                               "Alternatives"
                                               (list-hyps (:alts b))
                                               "Rejected"
                                               (list-hyps (mapcat :rej ars))}]))
                                         (range (count (:best wslog)))))
                 "Final" {"Accepted" (list-hyps (:accepted (:final wslog)))
                          "Rejected" (list-hyps (:rejected (:final wslog)))
                          "No explainers" (list-hyps (:no-explainers (:final wslog)))
                          "Unexplained" (list-hyps (:unexplained (:final wslog)))
                          "Unaccepted" (list-hyps (:unaccepted (:final wslog)))}})]
    (apply sorted-map-by (AlphanumComparator.)
           (mapcat (fn [ep] [(str ep) {"Workspace" (ws-fn (:log (:workspace ep)))
                                       "Meta-Log" nil}])
                   ep-states))))

(defn commas
  [hyps]
  (apply str (interpose ", " (sort (AlphanumComparator.) (map :id hyps)))))

(defn hyp-info
  [workspace hyp]
  (format (str "%s\n\nExplains: %s\n\nExplainers: %s\n\n"
               "Conflicts: %s\n\nApriori: %s\nConfidence: %s\n\nLog:\n%s")
          (:desc hyp)
          (commas (sort-by :id (:explains hyp)))
          (apply str
                 (interpose ", "
                            (map #(format "[%s]" %)
                                 (map commas                                      
                                      (vals (group-by :type
                                                      (ws/find-explainers workspace hyp)))))))
          (commas (ws/find-conflicts workspace hyp))
          (conf-str (:apriori hyp))
          (conf-str (ws/hyp-conf workspace hyp))
          (apply str (interpose "\n" (ws/hyp-log workspace hyp)))))

(defn final-explainers
  [workspace]
  (letfn [(expl-id-confs [expl] (map (fn [h] (format "%s (%.2f)" (:id h)
                                                     (ws/hyp-conf workspace h)))
                                     expl))
          (lines [ss] (apply str (interpose "\n" ss)))
          (expls [explainers]
            (lines (map (fn [{hyp :hyp expl :explainers}]
                          (format "%s: %s" (:id hyp)
                                  (apply str (interpose ", " (expl-id-confs expl)))))
                        explainers)))]
    (format "Final immediate explainers:\n\n%s\n\nFinal transitive explainers:\n\n%s"
            (expls (:last-immediate-explainers (:final (:log workspace))))
            (expls (:last-transitive-explainers (:final (:log workspace)))))))

(defn format-hyp-info
  [workspace hyp]
  (hyp-info workspace hyp))

(comment
  starts (set/difference (ws/get-hyps workspace) (:forced workspace))
  dep-analysis (apply str (map (fn [[s hyps]]
                                 (format "%s: %s\n" (:id s)
                                         (apply str (interpose ", " (map :id (sort-by :id hyps)))))) (filter (comp not-empty second) (analyze-dependency @or-state hyp starts)))))

(defn scroll-top
  [scroll]
  (javax.swing.SwingUtilities/invokeLater
   (proxy [Runnable] []
     (run [] (.. scroll (getVerticalScrollBar) (setValue 0))))))

(defn show-log
  [path]
  (if path
    (let [last-comp (node (. path getLastPathComponent))
          ;; find top-most ep-state
          ep-state (if (< 1 (. path getPathCount))
                     (if-let [ep-id (re-find #"^[A-Z]+"
                                             (str (. path getPathComponent 1)))]
                       (find-first #(= (:id %) ep-id) (flatten-est
                                                       (:est @or-state)))))
          ws (if ep-state (:workspace ep-state))]
      (if (= "Meta-Log" last-comp)
        (dosync (alter workspace-log (constantly (get (:meta-logs @or-state) (:id ep-state)))))
        (do
          (swap! workspace-selected (constantly ws))
          (let [hyp (if ws (find-first #(= (:id %) last-comp) (ws/get-hyps ws)))]
            (swap! hyp-selected (constantly hyp))
            (if hyp
              (dosync (alter workspace-log (constantly (format-hyp-info ws hyp))))
              (dosync (alter workspace-log (constantly (final-explainers ws))))))))
      (scroll-top @workspace-log-textbox))))

(defn show-analysis [])

(comment
  (defn show-analysis
    []
    (when (and @workspace-selected @hyp-selected)
      (let [ws @workspace-selected
            hyp @hyp-selected
            accepted? ((:accepted ws) hyp)
            [acc unacc rej] (ws/analyze ws hyp pdata)
            group-str (fn [type hyps]
                        (format "Hyp groups, when rejected, cause this hyp to be %s: %s"
                                type (apply str (interpose ", " (map #(format "[%s]" %)
                                                                     (map commas hyps))))))
            analysis (format "%s\n\n%s" (if accepted? (group-str "rejected" rej)
                                            (group-str "accepted" acc))
                             (group-str "unaccepted" unacc))]
        (dosync
         (alter workspace-log
                (fn [log] (format "%s\n\nAnalysis:\n\n%s"
                                  log analysis))))
        (scroll-top @workspace-log-textbox)))))

(defn update-logs
  []
  (dosync
   (alter truedata-log (constantly ((:get-truedata-log (:player-fns @problem)))))
   (alter problem-log (constantly ((:get-problem-log (:player-fns @problem)))))
   (alter hyp-choices
          (constantly (sort (AlphanumComparator.)
                            (map :id (ws/get-hyps (:workspace (cur-ep (:est @or-state))))))))
   (alter abduction-tree-map
          (constantly (build-abduction-tree-map @or-state))))
  (. problem-log-label setText (format "Problem log for: %s" (str (cur-ep (:est @or-state)))))
  (when (and @truedata-log-textbox @problem-log-textbox @workspace-log-textbox)
    (scroll-top @truedata-log-textbox)
    (scroll-top @problem-log-textbox)
    (scroll-top @workspace-log-textbox)))

(defn logs-tab
  []
  (dosync
   (alter truedata-log-textbox
          (constantly (scroll-panel
                       (doto (text-area :str-ref truedata-log
                                        :editable false :wrap true)
                         (.setFont (Font. "WenQuanYi Micro Hei" Font/PLAIN 14))))))
   (alter problem-log-textbox
          (constantly (scroll-panel
                       (doto (text-area :str-ref problem-log
                                        :editable false :wrap true)
                         (.setFont (Font. "WenQuanYi Micro Hei" Font/PLAIN 14))))))
   (alter workspace-log-textbox
          (constantly (scroll-panel
                       (doto (text-area :str-ref workspace-log
                                        :editable false :wrap true)
                         (.setFont (Font. "WenQuanYi Micro Hei" Font/PLAIN 14)))))))
  (doto (split-vertical
         @truedata-log-textbox
         (doto (split-vertical
                (panel :layout (GridBagLayout.)
                       :constrains (java.awt.GridBagConstraints.)
                       [:gridx 0 :gridy 0 :weightx 1.0 :weighty 0.0
                        :fill :BOTH :insets (Insets. 5 0 5 0)
                        _ problem-log-label
                        :gridy 1 :weighty 1.0
                        _ @problem-log-textbox])
                (doto (split-horizontal
                       (tree :name tr
                             :model (mapref-tree-model
                                     abduction-tree-map "Epistemic states")
                             :action ([_ _] (show-log (.getSelectionPath tr))))
                       (panel :layout (GridBagLayout.)
                              :constrains (java.awt.GridBagConstraints.)
                              [:gridx 0 :gridy 0 :weightx 1.0 :weighty 1.0 :gridwidth 2
                               :fill :BOTH :insets (Insets. 0 0 0 0)
                               _ @workspace-log-textbox
                               :gridx 0 :gridy 1 :weightx 1.0 :weighty 0.0 :gridwidth 1
                               _ (panel)
                               :gridx 1 :weightx 0.0
                               _ (button "Analyze" :action ([_] (show-analysis)))]))
                  (.setDividerLocation 300)))
           (.setDividerLocation 200)))
    (.setDividerLocation 200)))
