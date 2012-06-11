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
  (:use [retrospect.logging])
  (:use [retrospect.state]))

(def truedata-log (ref ""))
(def problem-log (ref ""))
(def problem-log-label (label ""))
(def hyp-selected (atom nil))
(def workspace-selected (atom nil))
(def hyp-id (ref ""))
(def hyp-apriori-label (label "Apriori:"))
(def hyp-confidence-label (label "Conf:"))
(def hyp-truefalse-label (label "T/F:"))
(def hyp-accepted-label (label "Acc:"))
(def hyp-explains (ref ""))
(def hyp-explainers (ref ""))
(def hyp-boosts (ref ""))
(def hyp-conflicts (ref ""))
(def hyp-log (ref ""))
(def abduction-tree-map (ref {}))

(def anc (AlphanumComparator.))

(defn list-hyps
  [hyps]
  (apply sorted-map-by anc (mapcat (fn [h] [(:name h) nil]) hyps)))

(defn build-cycle
  [wslog i]
  (let [b (nth (:best wslog) i)
        ;; seq of {:acc :rej} pairs (maps)
        ars (get (:accrej wslog) (inc i))]
    [(format "Cycle %d %s" (inc i) (if (:essential? b) "essential"
                                       (format "delta %.2f" (:delta b))))
     {"Best" {(:name (:best b)) nil}
      "Explained" {(:name (:explained b)) nil}
      "Accepted" (list-hyps (disj (set (map :acc ars)) (:best b)))
      "Rejected" (list-hyps (mapcat :rej ars))}]))

(defn build-abduction-tree-map
  [or-state]
  (let [est (:est or-state)
        ep-states (flatten-est est)        
        ws-fn (fn [ws time]
                (let [wslog (:log ws)
                      tf-fn (fn [hyp] ((:true-hyp?-fn (:abduction @problem))
                                       @truedata time hyp))]
                  {"Hypotheses"
                   (apply merge
                          (for [t (keys (:hypotheses ws))]
                            (let [acc-hyps (map #(ws/lookup-hyp ws %)
                                                (get (:accepted ws) t))
                                  not-acc-hyps (set/difference
                                                (set (map #(ws/lookup-hyp ws %)
                                                          (get (:hypotheses ws) t)))
                                                acc-hyps)
                                  acc-tf-hyps (group-by tf-fn acc-hyps)
                                  not-acc-tf-hyps (group-by tf-fn not-acc-hyps)]
                              {(name t)
                               {"All" (list-hyps (map #(ws/lookup-hyp ws %)
                                                      (get (:hypotheses ws) t)))
                                "Accepted"
                                {"True" (list-hyps (get acc-tf-hyps true))
                                 "False" (list-hyps (get acc-tf-hyps false))}
                                "Not accepted"
                                {"True" (list-hyps (get not-acc-tf-hyps true))
                                 "False" (list-hyps (get not-acc-tf-hyps false))}}})))
                   "Forced" (list-hyps (map #(ws/lookup-hyp ws %) (:forced ws)))
                   "Cycles" (apply sorted-map-by anc
                                   (mapcat #(build-cycle wslog %)
                                           (range (count (:best wslog)))))
                   "Accepted" (list-hyps (map #(ws/lookup-hyp ws %)
                                              (apply concat (vals (:accepted ws)))))
                   "No explainers" (list-hyps (ws/find-no-explainers ws))
                   "Unexplained" (list-hyps (ws/get-unexplained wslog))
                   "Unaccepted" (list-hyps (ws/find-unaccepted ws))}))]
    (apply sorted-map-by anc
           (mapcat (fn [ep] [(str ep) (assoc (ws-fn (:workspace ep) (:time ep)) "Log" nil)])
                   ep-states))))

(defn update-hyp-info
  [workspace time hyp]
  (let [alphanum (AlphanumComparator.)
        explains (str/join ", " (map str (sort-by :name alphanum (ws/explains hyp))))
        explainers (str/join ", " (map #(format "[%s]" %)
                                       (map #(str/join ", " (sort-by :name alphanum %))
                                            (vals (group-by :type
                                                            (map #(ws/lookup-hyp workspace %)
                                                                 (get (:explainers workspace)
                                                                      (:id hyp))))))))
        boosts (str/join ", " (map str (sort-by :name alphanum (:boosts hyp))))
        conflicts (str/join ", " (map str (sort-by :name alphanum
                                                   (ws/find-conflicts-all workspace hyp))))]
    (. hyp-apriori-label setText (format "Apriori: %.2f" (ws/lookup-score workspace hyp)))
    (. hyp-confidence-label setText (format "Conf: %.2f" (ws/hyp-conf workspace hyp)))
    (. hyp-truefalse-label setText
       (if ((:true-hyp?-fn (:abduction @problem)) @truedata time hyp) "TF: True" "TF: False"))
    (. hyp-accepted-label setText
       (if (ws/accepted? workspace hyp) "Acc: True" "Acc: False"))
    (dosync
     (alter hyp-id (constantly (:desc hyp)))
     (alter hyp-explains (constantly (str "Explains: " explains)))
     (alter hyp-explainers (constantly (str "Explainers: " explainers)))
     (alter hyp-boosts (constantly (str "Boosts: " boosts)))
     (alter hyp-conflicts (constantly (str "Conflicts: " conflicts)))
     (alter hyp-log (constantly (str/join "\n" (ws/hyp-log workspace hyp)))))))

(defn final-explainers
  [workspace]
  (letfn [(confs [expl] (map (fn [h] (format "%s (%.2f)" (:name h) (ws/hyp-conf workspace h)))
                             expl))]
    (format "Final explainers:\n\n%s"
            (str/join "\n" (map (fn [{hyp :hyp expl :expl}]
                                  (format "%s: %s" (:name hyp) (str/join ", " (confs expl))))
                                (:last-explainers (:log workspace)))))))

(defn show-log
  [path]
  (if path
    (let [last-comp (node (. path getLastPathComponent))
          ;; find top-most ep-state
          ep-state (if (< 1 (. path getPathCount))
                     (if-let [ep-id (re-find #"^[A-Z]+" (str (. path getPathComponent 1)))]
                       (find-first #(= (:id %) ep-id) (flatten-est (:est @or-state)))))
          ws (if ep-state (:workspace ep-state))]
      (when (not= "Log" last-comp)
        (swap! workspace-selected (constantly ws))
        (let [hyp (if ws (find-first #(= (:name %) last-comp) (vals (:hyp-ids ws))))]
          (swap! hyp-selected (constantly hyp))
          (when hyp (update-hyp-info ws (:time ep-state) hyp)))))))

(defn update-logs
  []
  (dosync
   (alter truedata-log (constantly ((:get-truedata-log (:player-fns @problem)))))
   (alter problem-log (constantly ((:get-problem-log (:player-fns @problem)))))
   (alter abduction-tree-map (constantly (build-abduction-tree-map @or-state))))
  (. problem-log-label setText (format "Problem log for: %s" (str (cur-ep (:est @or-state))))))

(defn log-box
  [str-ref]
  (scroll-panel
   (doto (text-area :str-ref str-ref :editable false :wrap true)
     (.setFont (Font. "WenQuanYi Micro Hei" Font/PLAIN 12)))))

(defn logs-tab
  []
  (doto (split-vertical
         (log-box truedata-log)
         (doto (split-vertical
                (panel :layout (GridBagLayout.)
                       :constrains (java.awt.GridBagConstraints.)
                       [:gridx 0 :gridy 0 :weightx 1.0 :weighty 0.0
                        :fill :BOTH :insets (Insets. 5 0 5 0)
                        _ problem-log-label
                        :gridy 1 :weighty 1.0
                        _ (log-box problem-log)])
                (doto (split-horizontal
                       (doto (tree :name tr
                                   :model (mapref-tree-model
                                           abduction-tree-map "Epistemic states")
                                   :action ([_ _] (show-log (.getSelectionPath tr))))
                         (.setFont (Font. "Sans" Font/PLAIN 10)))
                       (panel :layout (GridBagLayout.)
                              :constrains (java.awt.GridBagConstraints.)
                              [:gridx 0 :gridy 0 :gridwidth 4 :weightx 1.0 :weighty 1.0
                               :fill :BOTH :insets (Insets. 5 5 5 5)
                               _ (log-box hyp-id)

                               :gridy 1 :gridwidth 1 :weighty 0.0
                               _ hyp-apriori-label
                               :gridx 1
                               _ hyp-confidence-label
                               :gridx 2
                               _ hyp-truefalse-label
                               :gridx 3
                               _ hyp-accepted-label

                               :gridy 2 :gridx 0 :gridwidth 4 :weighty 1.0
                               _ (log-box hyp-explains)

                               :gridy 3
                               _ (log-box hyp-explainers)

                               :gridy 4
                               _ (log-box hyp-boosts)

                               :gridy 5
                               _ (log-box hyp-conflicts)

                               :gridy 6
                               _ (log-box hyp-log)]))
                  (.setDividerLocation 200)))
           (.setDividerLocation 100)))
    (.setDividerLocation 100)))
