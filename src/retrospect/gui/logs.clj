(ns retrospect.gui.logs
  (:import (java.awt GridBagLayout Insets Dimension))
  (:import (javax.swing Box))
  (:import (misc AlphanumComparator))
  (:use [clj-swing.label])
  (:use [clj-swing.text-field])
  (:use [clj-swing.tree])
  (:import (clj-swing.tree Pathed))
  (:use [clj-swing.panel])
  (:use [clojure.contrib.seq :only [find-first]])
  (:require [retrospect.workspaces :as ws])
  (:use [retrospect.epistemicstates :only
         [previous-ep-state flatten-ep-state-tree]])
  (:use [retrospect.confidences])
  (:use [retrospect.state]))

(def truedata-log (ref ""))
(def problem-log (ref ""))
(def problem-log-label (label ""))
(def hyp-choices (ref []))
(def hyp-selected (atom nil))
(def hyp-info (ref ""))
(def abduction-tree-map (ref {}))
(def workspace-log (ref ""))

(defn build-abduction-tree-map
  [ep-state-tree in-meta?]
  (let [list-hyps #(apply sorted-map-by (AlphanumComparator.)
                          (mapcat (fn [h] [(:id h) nil]) %))
        ep-states (flatten-ep-state-tree ep-state-tree)
        ws-fn (fn [wslog]
                {"Added" (list-hyps (map :hyp (:added wslog)))
                 "Forced" (list-hyps (:forced wslog))
                 "Cycles" (apply sorted-map-by (AlphanumComparator.)
                                 (mapcat (fn [i]
                                           (let [b (nth (:best wslog) i)
                                                 ;; seq of {:acc :rej} pairs (maps)
                                                 ars (get (:accrej wslog) (inc i))]
                                             [(format "Cycle %d (%s)" (inc i)
                                                      (if (:essential? b)
                                                        "essential"
                                                        (format "delta %.2f"
                                                                (:delta b))))
                                              {"Best"
                                               {(:id (:best b)) nil}
                                               "Accepted (trans)"
                                               (list-hyps (disj (set (map :acc ars))
                                                                (:best b)))
                                               "Alternatives"
                                               (list-hyps (:alts b))
                                               "Rejected"
                                               (list-hyps (mapcat :rej ars))}]))
                                         (range (count (:best wslog)))))
                 "Final" {"Accepted" (list-hyps (:accepted (:final wslog)))
                          "Rejected" (list-hyps (:rejected (:final wslog)))
                          "Shared explains" (list-hyps (:shared-explains (:final wslog)))
                          "Unexplained" (list-hyps (:unexplained (:final wslog)))
                          "Unaccepted" (list-hyps (:unaccepted (:final wslog)))}})]
    (apply sorted-map-by (AlphanumComparator.)
           (mapcat (fn [ep] [(str ep) {"Workspace" (ws-fn (:log (:workspace ep)))}])
                   ep-states))))

(defn hyp-info
  [workspace hyp]
  (format "%s\n\nExplains: %s\n\nExplainers: %s\n\nConflicts: %s\n\nApriori: %s\nConfidence: %s\n\nLog:\n%s"
          (:desc hyp)
          (apply str (interpose ", " (sort (AlphanumComparator.)
                                           (map (comp str :id)
                                                (ws/find-explains workspace hyp :static))))) 
          (apply str (interpose ", " (sort (AlphanumComparator.)
                                           (map (comp str :id)
                                                (ws/find-explainers workspace hyp :static)))))
          (apply str (interpose ", " (sort (AlphanumComparator.)
                                           (map (comp str :id)
                                                (ws/find-conflicts workspace hyp :static)))))
          (confidence-str (:apriori hyp))
          (confidence-str (ws/hyp-conf workspace hyp))
          (apply str (interpose "\n" (ws/hyp-log workspace hyp)))))

(defn show-log
  [path]
  (if path
    (let [last-comp (node (. path getLastPathComponent))
          ;; find top-most ep-state
          ep-state (if (< 1 (. path getPathCount))
                     (if-let [ep-id (re-find #"^[A-Z]+"
                                             (str (. path getPathComponent 1)))]
                       (find-first #(= (:id %) ep-id) (flatten-ep-state-tree
                                                       (:ep-state-tree @or-state)))))
          ws (if ep-state (:workspace ep-state))]
      (if-let [hyp (if ws (find-first #(= (:id %) last-comp) (ws/get-hyps ws :static)))]
        (dosync (alter workspace-log (constantly (hyp-info ws hyp))))
        (dosync (alter workspace-log (constantly "")))))))

(defn update-logs
  []
  (dosync
   (alter truedata-log (constantly ((:get-truedata-log (:player-fns @problem)))))
   (alter problem-log (constantly ((:get-problem-log (:player-fns @problem)))))
   (alter hyp-choices
          (constantly (sort (map :id (ws/get-hyps (:workspace (:ep-state @or-state)) :static)))))
   (alter abduction-tree-map
          (constantly (build-abduction-tree-map (:ep-state-tree @or-state) false))))
  (. problem-log-label setText (format "Problem log for: %s" (str (:ep-state @or-state)))))

(defn logs-tab
  []
  (doto (split-vertical
         (scroll-panel (text-area :str-ref truedata-log
                                  :editable false :wrap true))
         (doto (split-vertical
                (panel :layout (GridBagLayout.)
                       :constrains (java.awt.GridBagConstraints.)
                       [:gridx 0 :gridy 0 :weightx 1.0 :weighty 0.0
                        :fill :BOTH :insets (Insets. 5 0 5 0)
                        _ problem-log-label
                        :gridy 1 :weighty 1.0
                        _ (scroll-panel (text-area :str-ref problem-log
                                                   :editable false :wrap true))])
                (doto (split-horizontal
                       (tree :name tr
                             :model (mapref-tree-model
                                     abduction-tree-map "Epistemic states")
                             :action ([_ _] (show-log (.getSelectionPath tr))))
                       (scroll-panel (text-area :str-ref workspace-log
                                                :editable false :wrap true)))
                  (.setDividerLocation 200)))
           (.setDividerLocation 200)))
    (.setDividerLocation 200)))
