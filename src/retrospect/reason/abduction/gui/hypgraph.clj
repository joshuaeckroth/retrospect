(ns retrospect.reason.abduction.gui.hypgraph
  (:import (java.awt GridBagLayout Insets Graphics Dimension Color Font))
  (:import (java.awt.image BufferedImage))
  (:import (javax.swing JLabel ImageIcon JViewport))
  (:import (misc AlphanumComparator))
  (:use [clojure.contrib.seq-utils :only [find-first]])
  (:use [clj-swing.core :only [add-action-listener]])
  (:use [clj-swing.panel])
  (:use [clj-swing.button])
  (:use [clj-swing.label])
  (:require [clojure.string :as str])
  (:use [retrospect.gui.graphs])
  (:use [retrospect.state])
  (:use [retrospect.epistemicstates :only [cur-ep]])
  (:require [retrospect.reason.abduction.workspace :as ws])
  (:use [retrospect.reason.abduction.gui.logs :only [log-box]]))

(def canvas (ref nil))
(def hyp-id (ref ""))
(def hyp-id-label (doto (label "")
                    (.setFont (Font. "WenQuanYi Micro Hei" Font/PLAIN 12))))
(def hyp-score-label (label "Score:"))
(def hyp-truefalse-label (label "T/F:"))
(def hyp-accepted-label (label "Acc:"))
(def hyp-explains (ref ""))
(def hyp-explainers (ref ""))
(def hyp-conflicts (ref ""))
(def hyp-log (ref ""))

(def current-hypgraph-dot (ref ""))
(def current-hypgraph-svg (ref ""))

(defn listener
  [node]
  (let [workspace (:workspace (cur-ep (:est @or-state)))
        hyp (find-first #(= (:id %) node) (apply concat (vals (:hypotheses workspace))))]
    (when hyp
      (let [alphanum (AlphanumComparator.)
            explains (str/join ", " (map str (sort-by :id alphanum (:explains hyp))))
            explainers (str/join ", " (map #(format "[%s]" %)
                                           (map #(str/join ", " (sort-by :id alphanum %))
                                                (vals (group-by :type
                                                                (get (:explainers workspace)
                                                                     hyp))))))
            conflicts (str/join ", " (map str (sort-by :id alphanum
                                                       (ws/find-conflicts workspace hyp))))]
        (. hyp-id-label setText (format "%s %s" (:id hyp) (:short-str hyp)))
        (. hyp-score-label setText (format "Score: %.2f" (ws/lookup-score workspace hyp)))
        (. hyp-truefalse-label setText
           (if ((:true-hyp?-fn (:abduction @problem))
                @truedata (:time (cur-ep (:est @or-state))) hyp)
             "TF: True" "TF: False"))
        (dosync
         (alter hyp-id (constantly (:desc hyp)))
         (alter hyp-explains (constantly (str "Explains: " explains)))
         (alter hyp-explainers (constantly (str "Explainers: " explainers)))
         (alter hyp-conflicts (constantly (str "Conflicts: " conflicts)))
         (alter hyp-log (constantly (str/join "\n" (ws/hyp-log workspace hyp)))))))))

(defn generate-hypgraph
  []
  (let [ep (cur-ep (:est @or-state))
        hypgraph (:graph (ws/update-graph (:workspace ep)))]
    (generate-graph hypgraph @canvas listener false
                    current-hypgraph-dot current-hypgraph-svg)))

(defn hypgraph-tab
  []
  (dosync (alter canvas (constantly (create-canvas))))
  (panel :layout (GridBagLayout.)
         :constrains (java.awt.GridBagConstraints.)
         [:gridx 0 :gridy 0 :weightx 1.0 :weighty 1.0 :gridwidth 4 :fill :BOTH
          :insets (Insets. 5 5 5 5)
          _ @canvas
          :gridy 1 :gridwidth 1 :gridx 0 :weightx 0.25 :weighty 0.0
          _ hyp-id-label
          :gridx 1
          _ hyp-score-label
          :gridx 2
          _ hyp-truefalse-label
          :gridy 2 :gridx 0 :weighty 0.1
          _ (log-box hyp-id)
          :gridx 1
          _ (log-box hyp-explains)
          :gridx 2
          _ (log-box hyp-explainers)
          :gridx 3
          _ (log-box hyp-conflicts)
          :gridy 3 :gridx 0 :gridwidth 4 :weightx 1.0
          _ (log-box hyp-log)
          :gridy 4 :gridwidth 1 :gridx 0 :weighty 0.0
          _ (panel)
          :gridx 1
          _ (doto (button "Save Dot")
              (add-action-listener ([_] (save-dot @current-hypgraph-dot))))
          :gridx 2
          _ (doto (button "Save SVG")
              (add-action-listener ([_] (save-svg @current-hypgraph-svg))))
          :gridx 3
          _ (doto (button "Generate")
              (add-action-listener ([_] (generate-hypgraph))))]))

(defn update-hypgraph
  [])
