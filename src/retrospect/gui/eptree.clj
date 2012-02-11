(ns retrospect.gui.eptree
  (:import (java.awt.image BufferedImage))
  (:import (javax.swing JLabel ImageIcon JViewport))
  (:use [clj-swing.panel])
  (:use [retrospect.state])
  (:use [retrospect.epistemicstates :only [draw-est]]))

(defn get-ep-tree
  [est]
  (if est
    (JLabel. (ImageIcon. (draw-est est)))
    (JLabel. (ImageIcon. (BufferedImage. 1 1 (. BufferedImage TYPE_4BYTE_ABGR))))))

(def ep-tree-scroll
  (scroll-panel (get-ep-tree nil)))

(defn ep-tree-tab
  []
  ep-tree-scroll)

(defn update-ep-tree
  []
  (. ep-tree-scroll setViewport
     (doto (JViewport.) (.setView (get-ep-tree (:est @or-state))))))
