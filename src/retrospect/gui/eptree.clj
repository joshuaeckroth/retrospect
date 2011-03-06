(ns retrospect.gui.eptree
  (:import (java.awt.image BufferedImage))
  (:import (javax.swing JLabel ImageIcon JViewport))
  (:use [clj-swing.panel])
  (:use [retrospect.state])
  (:use [retrospect.epistemicstates :only [draw-ep-state-tree]]))

(defn get-ep-tree
  [ep-state-tree]
  (if ep-state-tree
    (JLabel. (ImageIcon. (draw-ep-state-tree ep-state-tree)))
    (JLabel. (ImageIcon. (BufferedImage. 1 1 (. BufferedImage TYPE_4BYTE_ABGR))))))

(def ep-tree-scroll
  (scroll-panel (get-ep-tree nil)))

(defn ep-tree-tab
  []
  ep-tree-scroll)

(defn update-ep-tree
  []
  (. ep-tree-scroll setViewport
     (doto (JViewport.) (.setView (get-ep-tree (:ep-state-tree @or-state))))))
