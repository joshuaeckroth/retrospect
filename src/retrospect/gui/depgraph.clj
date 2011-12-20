(ns retrospect.gui.depgraph
  (:import (java.awt GridBagLayout Insets Graphics Dimension Color))
  (:import (java.awt.image BufferedImage))
  (:import (javax.swing JLabel ImageIcon JViewport))
  (:import (org.apache.batik.swing JSVGCanvas))
  (:use [clj-swing.core :only [add-action-listener]])
  (:use [clj-swing.panel])
  (:use [clj-swing.button])
  (:use [retrospect.gui.graphs])
  (:use [retrospect.state])
  (:use [retrospect.epistemicstates :only
         [current-ep-state previous-ep-state]]))

(def canvas (JSVGCanvas.))

(defn generate-depgraph
  []
  (let [ep-state (let [ep (current-ep-state (:ep-state-tree @or-state))]
                   (if (re-find #"\?" (str ep))
                     (previous-ep-state (:ep-state-tree @or-state)) ep))
        depgraph (:depgraph ep-state)]
    (generate-graph depgraph canvas true)))

(defn depgraph-tab
  []
  (panel :layout (GridBagLayout.)
         :constrains (java.awt.GridBagConstraints.)
         [:gridx 0 :gridy 0 :weightx 1.0 :weighty 1.0 :gridwidth 2 :fill :BOTH
          :insets (Insets. 5 5 5 5)
          _ (scroll-panel canvas)
          :gridy 1 :gridwidth 1 :gridx 0 :weightx 1.0 :weighty 0.0
          _ (panel)
          :gridx 1 :weightx 0.0
          _ (doto (button "Generate")
              (add-action-listener ([_] (generate-depgraph))))]))

(defn update-depgraph
  [])
