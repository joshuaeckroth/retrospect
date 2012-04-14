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
  (:require [clojure.string :as str])
  (:require [retrospect.reason.abduction.workspace :as ws])
  (:use [retrospect.epistemicstates :only
         [cur-ep flatten-est]])
  #_(:use [retrospect.reason.abduction.robustness :only [analyze-dependency]])
  (:use [retrospect.confidences])
  (:use [retrospect.logging])
  (:use [retrospect.state]))

(def truedata-log (ref ""))
(def truedata-log-textbox (ref nil))
(def problem-log (ref ""))
(def problem-log-textbox (ref nil))
(def problem-log-label (label ""))
(def hyp-selected (atom nil))
(def workspace-selected (atom nil))
(def hyp-info (ref ""))
(def abduction-tree-map (ref {}))
(def workspace-log-textbox (ref nil))
(def workspace-log (ref ""))

(def anc (AlphanumComparator.))

(defn list-hyps
  [hyps]
  (apply sorted-map-by anc (mapcat (fn [h] [(:id h) nil]) hyps)))

(defn build-cycle
  [wslog i]
  (let [b (nth (:best wslog) i)
        ;; seq of {:acc :rej} pairs (maps)
        ars (get (:accrej wslog) (inc i))]
    [(format "Cycle %d %s" (inc i) (if (:essential? b) "essential"
                                       (format "delta %.2f" (:delta b))))
     {"Best" {(:id (:best b)) nil}
      "Explained" {(:id (:explained b)) nil}
      "Accepted" (list-hyps (disj (set (map :acc ars)) (:best b)))
      "Alternatives" (list-hyps (:alts b))
      "Rejected" (list-hyps (mapcat :rej ars))}]))

(defn build-abduction-tree-map
  [or-state]
  (let [est (:est or-state)
        ep-states (flatten-est est)
        ws-fn (fn [ws]
                (let [wslog (:log ws)]
                  {"Hypotheses" (list-hyps (apply concat (vals (:hypotheses ws))))
                   "Forced" (list-hyps (:forced ws))
                   "Cycles" (apply sorted-map-by anc
                                   (mapcat #(build-cycle wslog %)
                                           (range (count (:best wslog)))))
                   "Accepted" (list-hyps (apply concat (vals (:accepted ws))))
                   "Rejected" (list-hyps (apply concat (vals (:rejected ws))))
                   "No explainers" (list-hyps (:no-explainers wslog))
                   "Unexplained" (list-hyps (:unexplained wslog))
                   "Unaccepted" (list-hyps (:unaccepted wslog))}))]
    (apply sorted-map-by anc
           (mapcat (fn [ep] [(str ep) (assoc (ws-fn (:workspace ep)) "Log" nil)])
                   ep-states))))

(defn hyp-info
  [workspace time hyp]
  (let [explainers (vals (group-by :type (get (:explainers workspace) hyp)))]
    (format (str "%s\n\nExplains: %s\n\nExplainers: %s\n\n"
                 "Conflicts: %s\n\nApriori: %s\nConfidence: %s\n\n"
                 "This hyp is: %s\n\nLog:\n%s")
            (:desc hyp)
            (str/join ", " (sort-by :id (AlphanumComparator.) (:explains hyp)))
            (str/join ", " (map #(format "[%s]" %)
                                (map #(str/join ", " (sort-by :id (AlphanumComparator.) %)) explainers)))
            (str/join ", " (sort-by :id (AlphanumComparator.) (ws/find-conflicts workspace hyp)))
            (conf-str (:apriori hyp))
            (conf-str (ws/hyp-conf workspace hyp))
            (if ((:true-hyp?-fn (:abduction @problem)) @truedata time hyp) "True" "False")
            (str/join "\n" (ws/hyp-log workspace hyp)))))

(defn final-explainers
  [workspace]
  (letfn [(confs [expl] (map (fn [h] (format "%s (%.2f)" (:id h) (ws/hyp-conf workspace h)))
                             expl))]
    (format "Final explainers:\n\n%s"
            (str/join "\n" (map (fn [{hyp :hyp expl :expl}]
                                  (format "%s: %s" (:id hyp) (str/join ", " (confs expl))))
                                (:last-explainers (:log workspace)))))))

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
      (if (= "Log" last-comp)
        (dosync (alter workspace-log (constantly reason-log)))
        (do
          (swap! workspace-selected (constantly ws))
          (let [hyp (if ws (find-first #(= (:id %) last-comp)
                                       (apply concat (vals (:hypotheses ws)))))]
            (swap! hyp-selected (constantly hyp))
            (if hyp
              (dosync (alter workspace-log (constantly (hyp-info ws (:time ep-state) hyp))))
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
   (alter workspace-log (constantly "")))
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
                         (.setFont (Font. "WenQuanYi Micro Hei" Font/PLAIN 12))))))
   (alter problem-log-textbox
          (constantly (scroll-panel
                       (doto (text-area :str-ref problem-log
                                        :editable false :wrap true)
                         (.setFont (Font. "WenQuanYi Micro Hei" Font/PLAIN 12))))))
   (alter workspace-log-textbox
          (constantly (scroll-panel
                       (doto (text-area :str-ref workspace-log
                                        :editable false :wrap true)
                         (.setFont (Font. "WenQuanYi Micro Hei" Font/PLAIN 12)))))))
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
                       (panel :layout (GridBagLayout.)
                              :constrains (java.awt.GridBagConstraints.)
                              [:gridx 0 :gridy 0 :weightx 1.0 :weighty 1.0
                               :fill :BOTH :insets (Insets. 0 0 0 0)
                               _ (doto (tree :name tr
                                             :model (mapref-tree-model
                                                     abduction-tree-map "Epistemic states")
                                             :action ([_ _] (show-log (.getSelectionPath tr))))
                                   (.setFont (Font. "Sans" Font/PLAIN 10)))
                               :gridy 1 :weighty 0.0
                               _ (button "Update tree"
                                         :action
                                         ([_] (dosync (alter abduction-tree-map
                                                             (constantly (build-abduction-tree-map
                                                                          @or-state))))))])
                       (panel :layout (GridBagLayout.)
                              :constrains (java.awt.GridBagConstraints.)
                              [:gridx 0 :gridy 0 :weightx 1.0 :weighty 1.0 :gridwidth 2
                               :fill :BOTH :insets (Insets. 0 0 0 0)
                               _ @workspace-log-textbox
                               :gridx 0 :gridy 1 :weightx 1.0 :weighty 0.0 :gridwidth 1
                               _ (panel)
                               :gridx 1 :weightx 0.0
                               _ (button "Analyze" :action ([_] (show-analysis)))]))
                  (.setDividerLocation 200)))
           (.setDividerLocation 100)))
    (.setDividerLocation 100)))
