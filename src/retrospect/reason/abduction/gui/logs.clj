(ns retrospect.reason.abduction.gui.logs
  (:import (java.awt GridBagLayout Insets Dimension Font))
  (:import (javax.swing Box JScrollBar JTabbedPane))
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
  (:use [retrospect.state]))

(def truedata-log (ref ""))
(def problem-log (ref ""))
(def problem-log-label (label ""))
(def hyp-id (ref ""))
(def hyp-apriori-label (label "Apriori:"))
(def hyp-truefalse-label (label "T/F:"))
(def hyp-accepted-label (label "Acc:"))
(def hyp-explains (ref ""))
(def hyp-explainers (ref ""))
(def hyp-boosts (ref ""))
(def hyp-conflicts (ref ""))
(def hyp-log (ref ""))
(def reason-log (ref ""))
(def abduction-tree-map (ref {}))

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
         "Accepted" (list-hyps (:acc accrej))
         "Rejected" (list-hyps (:rej accrej))})))

(defn build-abduction-tree-map
  [est]
  (let [ep-states (flatten-est est)        
        ws-fn (fn [ws]
                (let [tf-fn (fn [hyp] ((:oracle-fn @problem)
                                      @truedata hyp))]
                  {"Hypotheses"
                   (apply merge
                          (for [t (keys (:hypotheses ws))]
                            (let [all-hyps (map #(ws/lookup-hyp ws %)
                                              (get (:hypotheses ws) t))
                                  acc-hyps (map #(ws/lookup-hyp ws %)
                                              (get (:accepted ws) t))
                                  not-acc-hyps (set/difference
                                                (set (map #(ws/lookup-hyp ws %)
                                                        (get (:hypotheses ws) t)))
                                                acc-hyps)
                                  all-tf-hyps (group-by tf-fn all-hyps)
                                  acc-tf-hyps (group-by tf-fn acc-hyps)
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
                                "Not accepted"
                                {"All" (list-hyps not-acc-hyps)
                                 "True" (list-hyps (get not-acc-tf-hyps true))
                                 "False" (list-hyps (get not-acc-tf-hyps false))}}})))
                   "Cycle" (build-cycle ws)
                   "No explainers" (list-hyps (map #(ws/lookup-hyp ws %)
                                                 (ws/get-no-explainers ws)))
                   "Unexplained" (list-hyps (map #(ws/lookup-hyp ws %)
                                               (ws/get-unexplained ws)))
                   "Unaccepted" (list-hyps (map #(ws/lookup-hyp ws %)
                                              (ws/find-unaccepted ws)))}))]
    (apply sorted-map-by anc
           (mapcat (fn [ep] [(str ep)
                            {"Workspace"
                             (assoc (ws-fn (:workspace ep)) "Log" nil)
                             "Abductive Meta"
                             (if-not (:meta-est ep) {}
                                     (build-abduction-tree-map (:meta-est ep)))}])
                   ep-states))))

(defn update-hyp-info
  [workspace hyp]
  (let [alphanum (AlphanumComparator.)
        explains (str/join ", " (map str (sort-by :name alphanum (ws/explains workspace hyp))))
        explainers (str/join ", " (map #(format "[%s]" %)
                                       (map #(str/join ", " (sort-by :name alphanum %))
                                            (vals (group-by :type
                                                            (map #(ws/lookup-hyp workspace %)
                                                                 (get (:explainers workspace)
                                                                      (:id hyp))))))))
        boosts (str/join ", " (map str (sort-by :name alphanum (:boosts hyp))))
        conflicts (str/join ", " (map str (sort-by :name alphanum
                                                 (ws/find-conflicts-all workspace hyp))))]
    (. hyp-apriori-label setText
       (format "Apriori: %.2f" (:apriori hyp)))
    (. hyp-truefalse-label setText
       (if ((:oracle-fn @problem) @truedata hyp)
         "TF: True" "TF: False"))
    (. hyp-accepted-label setText
       (if (ws/accepted? workspace hyp) "Acc: True" "Acc: False"))
    (dosync
     (alter hyp-id (constantly (:desc hyp)))
     (alter hyp-explains (constantly (str "Explains: " explains)))
     (alter hyp-explainers (constantly (str "Explainers: " explainers)))
     (alter hyp-boosts (constantly (str "Boosts: " boosts)))
     (alter hyp-conflicts (constantly (str "Conflicts: " conflicts)))
     (alter hyp-log (constantly (ws/hyp-log workspace hyp))))))

(defn show-log
  [path]
  (if path
    (let [last-comp (node (. path getLastPathComponent))
          ;; find top-most ep-state
          ep-state (if (> (. path getPathCount) 2)
                     (if-let [ep-id (re-find #"^\d+" (str (. path getPathComponent 1)))]
                       (find-first #(= (:id %) ep-id) (flatten-est (:est @or-state)))))
          meta-ep-state (if (and (> (. path getPathCount) 3)
                                 (= "Abductive Meta" (str (. path getPathComponent 2))))
                          (if-let [ep-id (re-find #"^\d+"
                                                  (str (. path getPathComponent 3)))]
                            (find-first #(= (:id %) ep-id)
                                        (flatten-est (:meta-est ep-state)))))
          ep (or meta-ep-state ep-state)
          ws (if ep (:workspace ep))]
      (if (= "Log" last-comp)
        (dosync (alter reason-log (constantly (str/join "\n" (reverse (:log ws))))))
        (let [hyp (if ws (find-first #(= (:name %) last-comp) (vals (:hyp-ids ws))))]
          (when hyp (update-hyp-info ws hyp)))))))

(defn update-logs
  []
  (dosync
   (alter truedata-log (constantly ((:get-truedata-log (:player-fns @problem)))))
   (alter problem-log (constantly ((:get-problem-log (:player-fns @problem)))))
   (alter abduction-tree-map (constantly (build-abduction-tree-map (:est @or-state)))))
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
                                  _ (log-box hyp-boosts)

                                  :gridy 5
                                  _ (log-box hyp-conflicts)

                                  :gridy 6
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
           (.setDividerLocation 100)))
    (.setDividerLocation 100)))
