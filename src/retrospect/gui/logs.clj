(ns retrospect.gui.logs
  (:import (java.awt GridBagLayout Insets))
  (:import (javax.swing Box))
  (:import (misc AlphanumComparator))
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
  [ep-state-tree in-meta?]
  (let [expand-meta-hyp (fn [h] (if (:data h)
                                  (if-let [est (:ep-state-tree (:data h))]
                                    (build-abduction-tree-map est true))))
        list-hyps #(apply sorted-map-by (AlphanumComparator.)
                          (mapcat (fn [h] [(:id h) (expand-meta-hyp h)]) %))
        ep-states (flatten-ep-state-tree ep-state-tree)
        ws-fn (fn [wslog]
                {"Added" (apply sorted-map-by (AlphanumComparator.)
                                (mapcat (fn [h] [(:id (:hyp h))
                                                 (expand-meta-hyp (:hyp h))])
                                        (:added wslog)))
                 "Forced" (list-hyps (:forced wslog))
                 "Cycles" (apply sorted-map-by (AlphanumComparator.)
                                 (mapcat (fn [i]
                                           (let [b (nth (:best wslog) i)
                                                 ar (nth (:accrej wslog) i)]
                                             [(format "Cycle %d" (inc i))
                                              {(format "Best%s" (if (:essential? b)
                                                                  " (essential)" ""))
                                               {(:id (:best b))
                                                (expand-meta-hyp (:best b))}
                                               "Alternatives" (list-hyps (:alts b))
                                               "Rejected" (list-hyps (:rej ar))
                                               "Penalized" (list-hyps (:penalized ar))}]))
                                         (range (count (:best wslog)))))
                 "Final" {"Accepted" (list-hyps (:accepted (:final wslog)))
                          "Rejected" (list-hyps (:rejected (:final wslog)))
                          "Shared explains" (list-hyps (:shared-explains (:final wslog)))
                          "Unexplained" (list-hyps (:unexplained (:final wslog)))}})
        ep-fn (fn [ep]
                (let [mw (if (or in-meta? (not (:meta-abduction @or-state))) {}
                             {"Meta Workspace"
                              (ws-fn (:log (get (:meta-workspaces @or-state) (:id ep))))})]
                  (merge mw {"Workspace" (ws-fn (:log (:workspace ep)))})))]
    (apply sorted-map-by (AlphanumComparator.)
           (mapcat (fn [ep] [(str ep) (ep-fn ep)]) ep-states))))

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
   (alter abduction-tree-map
          (constantly (build-abduction-tree-map (:ep-state-tree @or-state) false))))
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