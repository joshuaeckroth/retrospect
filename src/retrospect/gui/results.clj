(ns retrospect.gui.results
  (:import [misc WrapLayout])
  (:import (java.awt GridBagLayout FlowLayout Insets))
  (:import (javax.swing JViewport JTable UIManager BoxLayout))
  (:import (java.util Vector))
  (:use [clj-swing.panel])
  (:use [clj-swing.label])
  (:use [clj-swing.combo-box])
  (:use [clj-swing.button])
  (:require [clojure.contrib.math :as math])
  (:use [retrospect.state])
  (:use [retrospect.problem :only [get-headers]]))

(def headers-on (ref {}))

(def scroll (scroll-panel (JViewport.)))

(defn toggle-header
  [h]
  (dosync (alter headers-on #(assoc % h (not (h %))))))

(defn get-results-viewport
  [or-state]
  (let [headers (filter #(@headers-on %) (sort-by name (keys @headers-on)))
        results-matrix (map (fn [r] (map (fn [h] (h r)) headers))
                            (:results or-state))]
    (doto (JViewport.)
      (.setView  (JTable. (Vector. (map #(Vector. %) results-matrix))
                          (Vector. (sort (map name headers))))))))

(defn update-results
  []
  (. scroll (setViewport (get-results-viewport @or-state))))

(defn results-tab
  []
  (panel :layout (GridBagLayout.)
         :constrains (java.awt.GridBagConstraints.)
         [:gridx 0 :gridy 0 :weightx 1.0 :weighty 1.0 :gridwidth 5
          :fill :BOTH :insets (Insets. 5 5 5 5)
          _ scroll
          :gridx 0 :gridy 1 :weighty 0.5
          _ (let [cb (fn [h] (check-box :caption (name h)
                                        :selected false
                                        :action ([_] (toggle-header h)
                                                   (update-results))))
                  p-left (panel)
                  p-right (panel)
                  p (panel)
                  headers (sort-by name (get-headers @problem))]
              (.setLayout p (BoxLayout. p BoxLayout/X_AXIS))
              (.setLayout p-left (BoxLayout. p-left BoxLayout/Y_AXIS))
              (.setLayout p-right (BoxLayout. p-right BoxLayout/Y_AXIS))
              (.add p p-left)
              (.add p p-right)
              (doseq [h (take (math/ceil (/ (count headers) 2))
                              headers)]
                (doto p-left (.add (cb h))))
              (doseq [h (take-last (dec (math/ceil (/ (count headers) 2)))
                                   headers)]
                (doto p-right (.add (cb h))))
              (doto p-right (.add (panel)))
              (scroll-panel p))]))
