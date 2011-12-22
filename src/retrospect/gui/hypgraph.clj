(ns retrospect.gui.hypgraph
  (:import (java.awt GridBagLayout Insets Graphics Dimension Color))
  (:import (java.awt.image BufferedImage))
  (:import (javax.swing JLabel ImageIcon JViewport))
  (:use [clj-swing.core :only [add-action-listener]])
  (:use [clj-swing.panel])
  (:use [clj-swing.button])
  (:use [retrospect.gui.graphs])
  (:use [retrospect.state])
  (:use [retrospect.epistemicstates :only
         [current-ep-state previous-ep-state]]))

(def canvas (ref nil))

(defn listener
  [node]
  (println "got node" node))

(defn generate-hypgraph
  []
  (let [ep-state (let [ep (current-ep-state (:ep-state-tree @or-state))]
                   (if (re-find #"\?" (str ep))
                     (previous-ep-state (:ep-state-tree @or-state)) ep))
        hypgraph (:graph-static (:workspace ep-state))]
    (generate-graph hypgraph @canvas listener true)))

(defn hypgraph-tab
  []
  (dosync (alter canvas (constantly (create-canvas))))
  (panel :layout (GridBagLayout.)
         :constrains (java.awt.GridBagConstraints.)
         [:gridx 0 :gridy 0 :weightx 1.0 :weighty 1.0 :gridwidth 2 :fill :BOTH
          :insets (Insets. 5 5 5 5)
          _ @canvas
          :gridy 1 :gridwidth 1 :gridx 0 :weightx 1.0 :weighty 0.0
          _ (panel)
          :gridx 1 :weightx 0.0
          _ (doto (button "Generate")
              (add-action-listener ([_] (generate-hypgraph))))]))

(defn update-hypgraph
  [])
