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
  (:use [retrospect.workspaces :only [get-hyps hyp-conf]])
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
                                             [(format "Cycle %d (%s)" (inc i)
                                                      (if (:essential? b)
                                                        "essential"
                                                        (format "delta %.2f"
                                                                (:delta b))))
                                              {"Best"
                                               {(:id (:best b))
                                                (expand-meta-hyp (:best b))}
                                               "Alternatives" (list-hyps (:alts b))
                                               "Rejected" (list-hyps (:rej ar))
                                               "Penalized" (list-hyps (:penalized ar))}]))
                                         (range (count (:best wslog)))))
                 "Final" {"Accepted" (list-hyps (:accepted (:final wslog)))
                          "Rejected" (list-hyps (:rejected (:final wslog)))
                          "Shared explains" (list-hyps (:shared-explains (:final wslog)))
                          "Unexplained" (list-hyps (:unexplained (:final wslog)))
                          "Unaccepted" (list-hyps (:unaccepted (:final wslog)))}})
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
          ;; find top-most ep-state
          ep-state (if (< 1 (. path getPathCount))
                     (if-let [ep-id (re-find #"^[A-Z]+"
                                             (str (. path getPathComponent 1)))]
                       (find-first #(= (:id %) ep-id) (flatten-ep-state-tree
                                                       (:ep-state-tree @or-state)))))
          ;; is the selection below a meta-hyp?
          submeta-hyp-id (if-let [i (find-first
                                     (fn [i] (re-matches #"^MH.*"
                                                         (str (. path getPathComponent i))))
                                     (range (dec (. path getPathCount))))]
                           (str (. path getPathComponent i)))
          metahyp (if submeta-hyp-id
                    (find-first #(= (:id %) submeta-hyp-id)
                                (get-hyps (get (:meta-workspaces @or-state)
                                               (:id ep-state)))))
          meta-ep-state (if (and metahyp (:data metahyp) (:ep-state-tree (:data metahyp)))
                          (if-let [i (find-first
                                      (fn [i]
                                        (re-matches #"^[A-Z]+ \d.*"
                                                 (str (. path getPathComponent i))))
                                      (range 5 (. path getPathCount)))]
                            (find-first #(= (:id %)
                                            (re-find #"^[A-Z]+"
                                                     (str (. path getPathComponent i))))
                                        (flatten-ep-state-tree
                                         (:ep-state-tree (:data metahyp))))))
          ws (cond meta-ep-state (:workspace meta-ep-state)
                   (and (:meta-abduction @or-state)
                        (<= 3 (. path getPathCount))
                        (= "Meta Workspace" (str (. path getPathComponent 2))))
                   (get (:meta-workspaces @or-state) (:id ep-state))
                   ep-state (:workspace ep-state)
                   :else nil)]
      (if-let [hyp (if ws (find-first #(= (:id %) last-comp) (get-hyps ws)))]
        (dosync (alter workspace-log (constantly (hyp-info ws hyp))))
        (dosync (alter workspace-log (constantly "")))))))

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
