(ns retrospect.gui.logs
  (:import (java.awt GridBagLayout Insets))
  (:import (javax.swing Box))
  (:use [clj-swing.label])
  (:use [clj-swing.text-field])
  (:use [clj-swing.tree])
  (:import (clj-swing.tree Pathed))
  (:use [clj-swing.panel])
  (:use [clojure.contrib.seq :only [find-first]])
  (:use [retrospect.workspaces :only [get-hyps hyp-conf]])
  (:use [retrospect.epistemicstates :only [previous-ep-state flatten-ep-state-tree]])
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
  [or-state]
  (let [ep-states (flatten-ep-state-tree (:ep-state-tree or-state))
        ws-fn (fn [ws]
                {"Added" (apply sorted-map (mapcat (fn [h] [(:hypid h) nil])
                                                   (:added ws)))
                 "Forced" (apply sorted-map (mapcat (fn [h] [h nil]) (:forced ws)))
                 "Final" {"Accepted" (apply sorted-map
                                            (mapcat (fn [h] [h nil])
                                                    (:accepted (:final ws))))
                          "Rejected" (apply sorted-map
                                            (mapcat (fn [h] [h nil])
                                                    (:rejected (:final ws))))
                          "Shared explains" (apply sorted-map
                                                   (mapcat (fn [h] [h nil])
                                                           (:shared-explains (:final ws))))
                          "Unexplained" (apply sorted-map
                                               (mapcat (fn [h] [h nil])
                                                       (:unexplained (:final ws))))}})
        ep-fn (fn [ep]
                (let [mw (if-not (:meta-abduction or-state) {}
                                 {"Meta Workspace"
                                  (ws-fn (:log (get (:meta-workspaces or-state) (:id ep))))})]
                  (merge mw {"Workspace" (ws-fn (:log (:workspace ep)))})))]
    (apply sorted-map (mapcat (fn [ep] [(str ep) (ep-fn ep)]) ep-states))))

(defn hyp-info
  [workspace hyp]
  (format "%s\n\nApriori: %s\nConfidence: %s"
          (:desc hyp) (confidence-str (:apriori hyp))
          (confidence-str (hyp-conf workspace hyp))))

(defn show-log
  [path]
  (if path
    (let [last-comp (node (. path getLastPathComponent))
          ep-id (if (> (. path getPathCount) 1)
                  (re-find #"^[A-Z]+" (str (. path getPathComponent 1))))
          ep-state (if ep-id
                     (find-first #(= (:id %) ep-id)
                                 (flatten-ep-state-tree (:ep-state-tree @or-state))))
          hyp (if ep-state
                (find-first #(= (:id %) last-comp)
                            (concat (get-hyps (:workspace ep-state))
                                    (if-not (and (:meta-abduction @or-state)
                                                 (get (:meta-workspace @or-state)
                                                      (:id ep-state)))
                                      []
                                      (get-hyps (get (:meta-workspaces @or-state)
                                                     (:id ep-state)))))))
          ws (if (and hyp (re-matches #"^M.*" (:id hyp)))
               (get (:meta-workspaces @or-state) (:id ep-state))
               (:workspace ep-state))]
      (when hyp
        (dosync (alter workspace-log (constantly (hyp-info ws hyp))))))))

(defn update-logs
  []
  (dosync
   (alter truedata-log (constantly ((:get-truedata-log (:player-fns @problem)))))
   (alter problem-log (constantly ((:get-problem-log (:player-fns @problem)))))
   (alter hyp-choices
          (constantly (sort (map :id (get-hyps (:workspace (:ep-state @or-state)))))))
   (alter abduction-tree-map (constantly (build-abduction-tree-map @or-state))))
  (. problem-log-label setText (format "Problem log for: %s" (str (:ep-state @or-state)))))

(defn logs-tab
  []
  (split-vertical
   (scroll-panel (text-area :str-ref truedata-log :editable false))
   (split-vertical
    (panel :layout (GridBagLayout.) :constrains (java.awt.GridBagConstraints.)
           [:gridx 0 :gridy 0 :weightx 1.0 :weighty 0.0 :fill :BOTH :insets (Insets. 5 0 5 0)
            _ problem-log-label
            :gridy 1 :weighty 1.0
            _ (scroll-panel (text-area :str-ref problem-log :editable false))])
    (split-horizontal
     (tree :name tr
           :model (mapref-tree-model
                   abduction-tree-map "Epistemic states")
           :action ([_ _] (show-log (.getSelectionPath tr))))
     (scroll-panel (text-area :str-ref workspace-log :editable false))))))