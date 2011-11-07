(ns retrospect.gui.depgraph
  (:import (java.awt GridBagLayout Insets))
  (:import (java.awt.image BufferedImage))
  (:import (javax.swing JLabel ImageIcon JViewport))
  (:use [loom.graph :only [edges]])
  (:use [loom.io :only [dot-str]])
  (:use [clojure.java.shell :only [sh]])
  (:use [clj-swing.core :only [add-action-listener]])
  (:use [clj-swing.panel])
  (:use [clj-swing.button])
  (:use [retrospect.state])
  (:use [retrospect.epistemicstates :only [current-ep-state]]))

(defn get-depgraph
  [depgraph]
  (if (and depgraph (not-empty (edges depgraph)))
    (let [dot (dot-str depgraph :graph {:dpi 60 :rankdir "LR"})
          {png :out} (sh "dot" "-Tpng" :in dot :out-enc :bytes)]
      (JLabel. (ImageIcon. png)))
    (JLabel. (ImageIcon. (BufferedImage. 1 1 (. BufferedImage TYPE_4BYTE_ABGR))))))

(def depgraph-scroll
  (scroll-panel (get-depgraph nil)))

(defn generate-depgraph
  []
  (let [depgraph (:depgraph (current-ep-state (:ep-state-tree @or-state)))]
    (. depgraph-scroll setViewport
       (doto (JViewport.) (.setView (get-depgraph depgraph))))))

(defn depgraph-tab
  []
  (panel :layout (GridBagLayout.)
         :constrains (java.awt.GridBagConstraints.)
         [:gridx 0 :gridy 0 :weightx 1.0 :weighty 1.0 :gridwidth 2
          :insets (Insets. 5 5 5 5)
          _ depgraph-scroll
          :gridy 1 :gridwidth 1 :weightx 1.0 :weighty 0.0
          _ (panel)
          :gridx 1 :weightx 0.0
          _ (doto (button "Generate")
              (add-action-listener ([_] (generate-depgraph))))]))

(defn update-depgraph
  [])
