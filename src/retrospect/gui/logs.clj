(ns retrospect.gui.logs
  (:import (java.awt GridBagLayout Insets))
  (:use [clj-swing.label])
  (:use [clj-swing.text-field])
  (:use [clj-swing.tree])
  (:import (clj-swing.tree Pathed))
  (:use [clj-swing.panel])
  (:use [clojure.contrib.seq :only [find-first]])
  (:use [retrospect.workspaces :only [get-hyps]])
  (:use [retrospect.epistemicstates :only [previous-ep-state flatten-ep-state-tree]])
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
  [ep-states]
  (let [ws-fn (fn [ws] {"Added" (apply sorted-map (mapcat (fn [h] [(:hypid h) nil])
                                                          (:added ws)))
                        "Forced" (apply sorted-map (mapcat (fn [h] [h nil]) (:forced ws)))
                        "Final" {"Accepted" (apply sorted-map
                                                   (mapcat (fn [h] [h nil])
                                                           (:accepted (:final ws))))
                                 "Rejected" (apply sorted-map
                                                   (mapcat (fn [h] [h nil])
                                                           (:rejected (:final ws))))
                                 "Candidates" (apply sorted-map
                                                     (mapcat (fn [h] [h nil])
                                                             (:candidates (:final ws))))
                                 "Unexplained" (apply sorted-map
                                                      (mapcat (fn [h] [h nil])
                                                              (:unexplained (:final ws))))}})
        ep-fn (fn [ep] {"Workspace" (ws-fn (:log (:workspace ep)))})]
    (apply sorted-map (mapcat (fn [ep] [(str ep) (ep-fn ep)]) ep-states))))

(defn show-log
  [path]
  (let [last-comp (node (. path getLastPathComponent))
        ep-id (if (> (. path getPathCount) 1)
                (re-find #"^[A-Z]+" (str (. path getPathComponent 1))))
        ep-state (if ep-id (find-first #(= (:id %) ep-id)
                                       (flatten-ep-state-tree (:ep-state-tree @or-state))))
        hyp (if ep-state (find-first #(= (:id %) last-comp)
                                     (get-hyps (:workspace ep-state))))]
    (when hyp
      (dosync (alter workspace-log (constantly (:desc hyp)))))))

(defn update-logs
  []
  (let [est (:ep-state-tree @or-state)
        prev-ep (previous-ep-state est)
        ep-states (flatten-ep-state-tree est)]
    (dosync
     (alter truedata-log (constantly ((:get-truedata-log (:player-fns @problem)))))
     (alter problem-log (constantly ((:get-problem-log (:player-fns @problem)))))
     (alter hyp-choices
            (constantly (sort (map :id (get-hyps (:workspace (:ep-state @or-state)))))))
     (alter abduction-tree-map (constantly (build-abduction-tree-map ep-states)))))
  (. problem-log-label setText (format "Problem log for: %s" (str (:ep-state @or-state)))))

(comment
  (defn update-hyp-box
    []
    (if *ep-state*
      (if-let [hyp (if-let [choice (.getSelectedItem *hyp-choice*)]
                     (find-first #(= (:id %) choice) (get-hyps (:workspace *ep-state*))))]
        (. *hyp-box* setText
           (apply str (:desc hyp) "\n\n"
                  (interpose
                   "\n" (map str (filter (fn [l] (some #(= % (:id hyp)) (:hyp-ids l)))
                                         (:abducer-log (:workspace *ep-state*))))))))
      (. *hyp-box* setText ""))))

(defn logs-tab
  []
  (panel :layout (GridBagLayout.)
         :constrains (java.awt.GridBagConstraints.)
         [:gridx 0 :gridy 0 :weightx 1.0 :weighty 1.0 :fill :BOTH :insets (Insets. 5 5 5 5)
          _ (text-field :str-ref truedata-log)

          :gridx 0 :gridy 1 :weighty 0.0
          _ problem-log-label
          :gridx 0 :gridy 2 :weighty 1.0
          _ (text-field :str-ref problem-log)

          :gridx 0 :gridy 3
          _ (split-horizontal
             (tree :name tr
                   :model (mapref-tree-model
                           abduction-tree-map "Epistemic states")
                   :action ([_ _] (show-log (.getSelectionPath tr))))
             (text-field :str-ref workspace-log))]))