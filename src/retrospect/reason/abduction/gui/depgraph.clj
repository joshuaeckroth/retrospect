(ns retrospect.reason.abduction.gui.depgraph
  (:import (java.awt GridBagLayout Insets Graphics Dimension Color))
  (:import (java.awt.image BufferedImage))
  (:import (javax.swing JLabel ImageIcon JViewport))
  (:import (org.apache.batik.swing JSVGCanvas))
  (:use [clj-swing.core :only [add-action-listener]])
  (:use [clj-swing.panel])
  (:use [clj-swing.button])
  (:use [retrospect.gui.graphs])
  (:use [retrospect.state])
  (:use [retrospect.epistemicstates :only [cur-ep]]))

(def canvas (ref nil))

(defn listener
  [_])

(defn generate-depgraph
  []
  (let [ep (cur-ep (:est @or-state))
        depgraph (:depgraph ep)]
    (generate-graph depgraph @canvas listener true)))

(defn depgraph-tab
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
              (add-action-listener ([_] (generate-depgraph))))]))

(defn update-depgraph
  [])
