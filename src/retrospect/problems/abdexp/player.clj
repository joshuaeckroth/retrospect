(ns retrospect.problems.abdexp.player
  (:import (java.awt GridBagLayout Insets))
  (:use [loom.graph :only [digraph]])
  (:use [clj-swing.core :only [add-action-listener]])
  (:use [clj-swing.button])
  (:use [clj-swing.panel])
  (:use [retrospect.gui.graphs])
  (:use [retrospect.problems.abdexp.expgraph :only [format-dot-expgraph]])
  (:use [retrospect.epistemicstates :only [cur-ep]])
  (:use [retrospect.state]))

(def canvas (ref nil))

(def current-expgraph-dot (ref ""))
(def current-expgraph-svg (ref ""))

(defn listener
  [node])

(defn player-get-stats-panel
  []
  (panel))

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
