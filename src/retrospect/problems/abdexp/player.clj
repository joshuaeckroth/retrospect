(ns retrospect.problems.abdexp.player
  (:import (java.awt GridBagLayout Insets))
  (:require [clojure.string :as str])
  (:use [loom.graph :only [digraph]])
  (:use [clj-swing.core :only [add-action-listener]])
  (:use [clj-swing.button])
  (:use [clj-swing.panel])
  (:use [clj-swing.label])
  (:use [retrospect.gui.graphs])
  (:use [retrospect.problems.abdexp.expgraph :only [format-dot-expgraph]])
  (:use [retrospect.epistemicstates :only [cur-ep]])
  (:use [retrospect.reason.abduction.workspace :only [lookup-hyp]])
  (:use [retrospect.state]))

(def canvas (ref nil))

(def current-expgraph-dot (ref ""))
(def current-expgraph-svg (ref ""))

(def tpr-label (label ""))
(def fpr-label (label ""))
(def f1-label (label ""))
(def noexp-label (label ""))

(defn listener
  [node])

(defn player-get-stats-panel
  []
  (panel :layout (GridBagLayout.)
         :constrains (java.awt.GridBagConstraints.)
         [:gridx 0 :gridy 0 :weightx 1.0 :weighty 0.0
          :fill :BOTH :insets (Insets. 5 5 5 5)
          _ (label "TPR:")
          :gridx 1
          _ tpr-label
          :gridx 0 :gridy 1
          _ (label "FPR:")
          :gridx 1
          _ fpr-label
          :gridx 0 :gridy 2
          _ (label "F1:")
          :gridx 1
          _ f1-label
          :gridx 0 :gridy 3
          _ (label "NoExplainers:")
          :gridx 1
          _ noexp-label]))

(defn player-update-stats
  []
  (if-let [results (last (:results (cur-ep (:est @or-state))))]
    (do
      (. tpr-label (setText (format "%.2f" (:TPR results))))
      (. fpr-label (setText (format "%.2f" (:FPR results))))
      (. f1-label (setText (format "%.2f" (:F1 results))))
      (. noexp-label (setText (format "%.2f" (:NoExplainersPct results)))))
    (do
      (. tpr-label (setText "N/A"))
      (. fpr-label (setText "N/A"))
      (. f1-label (setText "N/A"))
      (. noexp-label (setText "N/A")))))

(defn player-update-diagram
  []
  (if (< 0 @time-now)
    (generate-graph (format-dot-expgraph (get (:test @truedata) @time-now))
                    @canvas listener false
                    current-expgraph-dot current-expgraph-svg)
    (generate-graph (digraph) @canvas listener false
                    current-expgraph-dot current-expgraph-svg)))

(defn player-setup-diagram
  []
  (dosync (alter canvas (constantly (create-canvas))))
  (panel :layout (GridBagLayout.)
         :constrains (java.awt.GridBagConstraints.)
         [:gridx 0 :gridy 0 :weightx 1.0 :weighty 1.0 :gridwidth 3 :fill :BOTH
          :insets (Insets. 5 5 5 5)
          _ @canvas
          :gridy 1 :gridx 0 :weightx 1.0 :weighty 0.0 :gridwidth 1
          _ (panel)
          :gridx 1 :weightx 0.0
          _ (doto (button "Save Dot")
              (add-action-listener ([_] (save-dot @current-expgraph-dot))))
          :gridx 2
          _ (doto (button "Save SVG")
              (add-action-listener ([_] (save-svg @current-expgraph-svg))))]))

(defn player-get-truedata-log
  []
  ;; TODO: fix so it's not specific to abduction
  (if (<= @time-now 0) ""
      (let [true-vertices (get (:true-vertices @truedata) @time-now)]
        (str "True vertices: "
             (str/join ", " (sort true-vertices))))))

(defn player-get-problem-log
  []
  (if (<= @time-now 0) ""
      (let [ws (:workspace (cur-ep (:est @or-state)))]
        (str "Believed vertices: "
             (str/join ", " (sort (map #(:vertex (lookup-hyp ws %))
                                     (get (:accepted ws) :expl))))))))
