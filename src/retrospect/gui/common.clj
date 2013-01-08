(ns retrospect.gui.common
  (:use [clj-swing.text-field])
  (:use [clj-swing.panel]))

(defn log-box
  [str-ref]
  (scroll-panel (text-area :str-ref str-ref :editable false :wrap true)))
