(ns retrospect.gui.depgraph
  (:import (java.awt.image BufferedImage))
  (:import (javax.swing JLabel ImageIcon JViewport))
  (:use [loom.graph :only [edges]])
  (:use [loom.io :only [dot-str]])
  (:use [clojure.java.shell :only [sh]])
  (:use [clj-swing.panel])
  (:use [retrospect.state])
  (:use [retrospect.epistemicstates :only [draw-depgraph current-ep-state]]))

(defn get-depgraph
  [depgraph]
  (if (and depgraph (not-empty (edges depgraph)))
    (let [dot (dot-str depgraph :graph {:dpi 60 :rankdir "LR"})
          {png :out} (sh "dot" "-Tpng" :in dot :out-enc :bytes)]
      (JLabel. (ImageIcon. png)))
    (JLabel. (ImageIcon. (BufferedImage. 1 1 (. BufferedImage TYPE_4BYTE_ABGR))))))

(def depgraph-scroll
  (scroll-panel (get-depgraph nil)))

(defn depgraph-tab
  []
  depgraph-scroll)

(defn update-depgraph
  []
  (let [depgraph (:depgraph (current-ep-state (:ep-state-tree @or-state)))]
    (. depgraph-scroll setViewport
       (doto (JViewport.) (.setView (get-depgraph depgraph))))))
