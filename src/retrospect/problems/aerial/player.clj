(ns retrospect.problems.aerial.player
  (:import (java.awt Dimension GridBagLayout GridBagConstraints Insets))
  (:use [clj-swing.panel])
  (:use [clj-swing.label])
  (:use [retrospect.epistemicstates :only [cur-ep]])
  (:use [retrospect.state]))

(def recall-label (label ""))
(def prec-label (label ""))
(def unexp-label (label ""))
(def noexp-label (label ""))

(defn player-update-diagram
  [])

(defn player-setup-diagram
  []
  (panel))

(defn player-get-stats-panel
  []
  (panel :layout (GridBagLayout.)
         :constrains (java.awt.GridBagConstraints.)
         [:gridx 0 :gridy 0 :weightx 1.0 :weighty 0.0
          :fill :BOTH :insets (Insets. 5 5 5 5)
          _ (label "Prec:")
          :gridx 1
          _ prec-label
          :gridx 0 :gridy 1
          _ (label "Recall:")
          :gridx 1
          _ recall-label
          :gridx 0 :gridy 2
          _ (label "Unexplained:")
          :gridx 1
          _ unexp-label
          :gridx 0 :gridy 3
          _ (label "NoExplainers:")
          :gridx 1
          _ noexp-label]))

(defn player-update-stats
  []
  (if-let [results (last (:results (cur-ep (:est @or-state))))]
    (do
      (. recall-label (setText (format "%.2f" (:Recall results))))
      (. prec-label (setText (format "%.2f" (:Prec results))))
      (. unexp-label (setText (format "%.2f" (:UnexplainedPct results))))
      (. noexp-label (setText (format "%.2f" (:NoExplainersPct results)))))
    (do
      (. recall-label (setText "N/A"))
      (. prec-label (setText "N/A"))
      (. unexp-label (setText "N/A"))
      (. noexp-label (setText "N/A")))))

(defn player-get-truedata-log
  [])

(defn player-get-problem-log
  [])
