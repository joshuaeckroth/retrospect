(ns retrospect.reason.abduction.gui.abduction-log
  (:import (java.awt GridBagLayout Insets Dimension Font))
  (:import (javax.swing Box JScrollBar JTabbedPane))
  (:import (misc AlphanumComparator))
  (:use [clj-swing.label])
  (:use [clj-swing.text-field])
  (:use [clj-swing.tree])
  (:use [clj-swing.button])
  (:use [clj-swing.panel])
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:require [retrospect.reason.abduction.workspace :as ws])
  (:use [retrospect.epistemicstates :only
         [cur-ep flatten-est]])
  (:use [retrospect.reason.abduction.evaluate :only
         [true-meta-hyp? find-meta-hyps group-hyps-by-true-false classify-error classify-noexp-reason]])
  #_(:use [retrospect.reason.abduction.robustness :only [analyze-dependency]])
  (:use [retrospect.gui.common])
  (:use [retrospect.state]))

(def hyp-id (ref ""))
(def hyp-apriori-label (label "Apriori:"))
(def hyp-truefalse-label (label "T/F:"))
(def hyp-accepted-label (label "Acc:"))
(def hyp-explains (ref ""))
(def hyp-explainers (ref ""))
(def hyp-conflicts (ref ""))
(def hyp-log (ref ""))
(def reason-log (ref ""))
(def abduction-tree-map (ref {}))
(def hyps-true-false (ref nil))
(def meta-hyps-true-false (ref nil))

(def anc (AlphanumComparator.))

(defn list-hyps
  [hyps]
  (apply sorted-map-by anc (mapcat (fn [h] [(:name h) nil]) hyps)))

(defn build-cycle
  [workspace]
  (let [accrej (:accrej workspace)]
    (if (empty? accrej) {}
        {(format "Best %s" (if (:essential? accrej) "essential"
                          (format "delta %.2f" (:delta accrej))))
         {(:name (:best accrej)) nil}
         "Explained" {(:name (:explained accrej)) nil}
         "Alternatives" (list-hyps (:alts accrej))
         "Normalized Aprioris"
         (apply hash-map
                (apply concat
                       (map (fn [i]
                            (let [a (nth (:normalized-aprioris accrej) i)]
                              [(format "%d: %.2f" i a) nil]))
                          (range (count (:normalized-aprioris accrej))))))
         "Accepted" (list-hyps (map #(ws/lookup-hyp workspace %) (:acc accrej)))
         "Rejected" (list-hyps (map #(ws/lookup-hyp workspace %) (:rej accrej)))})))

(defn build-abduction-tree-map
  [est meta?]
  (let [ep-states (flatten-est est)        
        ws-fn (fn [ws]
                (let [tf-fn (fn [hyp] (if meta?
                                       (true-meta-hyp? @truedata hyp)
                                       ((:oracle-fn @problem) @truedata hyp)))]
                  {"Hypotheses"
                   (apply merge
                          (for [t (keys (:hypotheses ws))]
                            (let [all-hyps (get (ws/hypotheses ws) t)
                                  acc-hyps (get (ws/accepted ws) t)
                                  rej-hyps (get (ws/rejected ws) t)
                                  ;; TODO: fix group-by or not-group-by
                                  not-acc-hyps (get (ws/undecided ws) t)
                                  all-tf-hyps (group-by tf-fn all-hyps)
                                  acc-tf-hyps (group-by tf-fn acc-hyps)
                                  rej-tf-hyps (group-by tf-fn rej-hyps)
                                  not-acc-tf-hyps (group-by tf-fn not-acc-hyps)]
                              {(name t)
                               {"All" 
                                {"All" (list-hyps all-hyps)
                                 "True" (list-hyps (get all-tf-hyps true))
                                 "False" (list-hyps (get all-tf-hyps false))}
                                "Accepted"
                                {"All" (list-hyps acc-hyps)
                                 "True" (list-hyps (get acc-tf-hyps true))
                                 "False" (list-hyps (get acc-tf-hyps false))}
                                "Rejected"
                                {"All" (list-hyps rej-hyps)
                                 "True" (list-hyps (get rej-tf-hyps true))
                                 "False" (list-hyps (get rej-tf-hyps false))}
                                "Undecided"
                                {"All" (list-hyps not-acc-hyps)
                                 "True" (list-hyps (get not-acc-tf-hyps true))
                                 "False" (list-hyps (get not-acc-tf-hyps false))}}})))
                   "Cycle" (build-cycle ws)
                   "No explainers" (list-hyps (ws/no-explainers ws))
                   "Unexplained" (list-hyps (ws/unexplained ws))}))]
    (apply sorted-map-by anc
           (mapcat (fn [ep]
                     (let [tree {"Workspace" (assoc (ws-fn (:workspace ep)) "Log" nil)}]
                       [(str ep)
                        (if-not (:meta-est ep) tree
                                (assoc tree
                                  "Abductive Meta"
                                  (build-abduction-tree-map (:meta-est ep) true)))]))
                   ep-states))))

(defn update-hyp-info
  [workspace hyp meta?]
  (let [alphanum (AlphanumComparator.)
        explains (str/join "\n" (map str (sort-by :name alphanum (ws/explains workspace hyp))))
        explainers (str/join "\n" (map #(format "\n%s\n" %)
                                     (map #(str/join "\n" (sort-by :name alphanum %))
                                        (vals (group-by :type (ws/explainers workspace hyp))))))
        conflicts (str/join "\n" (map str (sort-by :name alphanum
                                                 (ws/find-conflicts workspace hyp))))
        noexp? ((set (ws/no-explainers workspace)) hyp)
        meta-hyp? ((:meta-hyp-types @reasoner) (:type hyp))
        error (classify-error workspace (if meta-hyp? @meta-hyps-true-false @hyps-true-false) hyp)]
    (. hyp-apriori-label setText
       (format "Apriori: %.2f" (:apriori hyp)))
    (. hyp-truefalse-label setText
       (if (or (and meta? (true-meta-hyp? @truedata hyp))
               (and (not meta?) ((:oracle-fn @problem) @truedata hyp)))
         "TF: True" "TF: False"))
    (. hyp-accepted-label setText
       (if (ws/accepted? workspace hyp) "Acc: True" "Acc: False"))
    (dosync
     (alter hyp-id (constantly (:desc hyp)))
     (alter hyp-explains (constantly (str "Explains:\n" explains)))
     (alter hyp-explainers (constantly (str "Explainers:\n" explainers)))
     (alter hyp-conflicts (constantly (str "Conflicts:\n" conflicts)))
     (alter hyp-log (constantly (format "%s\n%s\n%s" (ws/hyp-log workspace hyp)
                                   (if (or (not noexp?) (and meta? (not meta-hyp?))) ""
                                       (format "Noexp reason: %s" (name (classify-noexp-reason workspace hyp))))
                                   (format "Error reason: %s" (name error))))))))

(defn show-log
  [path]
  (if path
    (let [last-comp (node (. path getLastPathComponent))
          ;; find top-most ep-state
          ep-state (if (> (. path getPathCount) 2)
                     (if-let [ep-id (re-find #"^\d+" (str (. path getPathComponent 1)))]
                       (first (filter #(= (:id %) ep-id) (flatten-est (:est @or-state))))))
          meta-ep-state (if (and (> (. path getPathCount) 3)
                                 (= "Abductive Meta" (str (. path getPathComponent 2))))
                          (if-let [ep-id (re-find #"^\d+"
                                                  (str (. path getPathComponent 3)))]
                            (first (filter #(= (:id %) ep-id)
                                      (flatten-est (:meta-est ep-state))))))
          ep (or meta-ep-state ep-state)
          ws (if ep (:workspace ep))]
      (if (= "Log" last-comp)
        (dosync (alter reason-log (constantly (str/join "\n" (reverse (:log ws))))))
        (let [hyp (if ws (first (filter #(= (:name %) last-comp) (vals (:hyp-ids ws)))))]
          (when hyp (update-hyp-info ws hyp (not (nil? meta-ep-state)))))))))

(defn update-abduction-log
  []
  (let [ws (:workspace (cur-ep (:est @or-state)))
        meta-hyps (find-meta-hyps (:est @or-state))]
    (dosync
     (alter abduction-tree-map (constantly (build-abduction-tree-map (:est @or-state) false)))
     (alter hyps-true-false (constantly (group-hyps-by-true-false (vals (:hyp-ids ws))
                                                                  :type @truedata (:oracle-fn @problem) false)))
     (alter meta-hyps-true-false (constantly (group-hyps-by-true-false meta-hyps :type
                                                                       @truedata true-meta-hyp? true))))))

(defn abduction-log-tab
  []
  (doto
      (split-horizontal
       (doto (tree :name tr
                   :model (mapref-tree-model
                           abduction-tree-map "Epistemic states")
                   :action ([_ _] (show-log (.getSelectionPath tr))))
         (.setFont (Font. "Sans" Font/PLAIN 10)))
       (doto (JTabbedPane.)
         (.addTab
          "Hyp Info"
          (panel :layout (GridBagLayout.)
                 :constrains (java.awt.GridBagConstraints.)
                 [:gridx 0 :gridy 0 :gridwidth 3 :weightx 1.0 :weighty 1.0
                  :fill :BOTH :insets (Insets. 5 5 5 5)
                  _ (log-box hyp-id)

                  :gridy 1 :gridwidth 1 :weighty 0.0
                  _ hyp-apriori-label
                  :gridx 1
                  _ hyp-truefalse-label
                  :gridx 2
                  _ hyp-accepted-label

                  :gridy 2 :gridx 0 :gridwidth 5 :weighty 1.0
                  _ (log-box hyp-explains)

                  :gridy 3
                  _ (log-box hyp-explainers)

                  :gridy 4
                  _ (log-box hyp-conflicts)

                  :gridy 5
                  _ (log-box hyp-log)]))
         (.addTab
          "Reason Log"
          (panel :layout (GridBagLayout.)
                 :constrains (java.awt.GridBagConstraints.)
                 [:gridx 0 :gridy 0 :weightx 1.0 :weighty 1.0
                  :fill :BOTH :insets (Insets. 5 5 5 5)
                  _ (log-box reason-log)]))
         (.setSelectedIndex 0)))
    (.setDividerLocation 200)))
