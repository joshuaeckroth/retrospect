(ns retrospect.gui.results
  (:import [misc WrapLayout])
  (:import (java.awt GridBagLayout FlowLayout Insets))
  (:import (javax.swing JViewport JTable))
  (:import (java.util Vector))
  (:use [clj-swing.panel])
  (:use [clj-swing.label])
  (:use [clj-swing.combo-box])
  (:use [clj-swing.button])
  (:use [incanter.core :only [to-list with-data dataset nrow]])
  (:use [incanter.charts :only [scatter-plot]])
  (:use [retrospect.state])
  (:use [retrospect.problem :only [get-headers]]))

(def x-selected (atom nil))
(def y-selected (atom nil))
(def headers-on (ref {}))
(def graph (ref nil))

(def graph-panel
  (panel :preferred-size [300 300]
         :paint ([g] (if @graph (.draw @graph g (.getClipBounds g))))))

(def scroll (scroll-panel (JViewport.)))

(defn toggle-header
  [h]
  (dosync (alter headers-on #(assoc % h (not (h %))))))

(defn get-results-viewport
  [or-state]
  (let [headers (filter #(@headers-on %) (keys @headers-on))
        results-matrix (map (fn [r] (map (fn [h] (h r)) headers))
                            (:results or-state))]
    (doto (JViewport.)
      (.setView  (JTable. (Vector. (map #(Vector. %) (to-list results-matrix)))
                          (Vector. (map name headers)))))))

(defn update-results
  []
  (. scroll (setViewport (get-results-viewport @or-state))))

(defn results-to-dataset
  [or-state]
  (let [headers (keys @headers-on)]
    (if (empty? headers) []
        (dataset headers (map (fn [r] (map (fn [h] (get r h)) headers))
                              (:results or-state))))))

(defn update-results-graph
  []
  (let [data (results-to-dataset @or-state)]
    (dosync
     (if (< 1 (nrow data))
       (alter graph
              (constantly
               (let [x-axis (keyword x-selected)
                     y-axis (keyword y-selected)]
                 (when (or x-axis y-axis)
                   (with-data data
                     (scatter-plot x-axis y-axis
                                   :x-label (name x-axis)
                                   :y-label (name y-axis)))))))
       (alter graph (constantly nil)))))
  (.repaint graph-panel))

(defn results-tab
  []
  (panel :layout (GridBagLayout.)
         :constrains (java.awt.GridBagConstraints.)
         [:gridx 0 :gridy 0 :weightx 1.0 :weighty 1.0 :gridwidth 5
          :fill :BOTH :insets (Insets. 5 5 5 5)
          _ scroll
          :gridx 0 :gridy 1 :weightx 0.0 :weighty 0.0 :gridwidth 1
          _ (label "x-axis:")
          :gridx 1
          _ (combo-box [] :model (seq-ref-combobox-model
                                  (ref (map name (get-headers @problem)))
                                  x-selected)
                       :action ([_] (update-results-graph)))
          :gridx 2
          _ (label "y-axis:")
          :gridx 3
          _ (combo-box [] :model (seq-ref-combobox-model
                                  (ref (map name (get-headers @problem)))
                                  y-selected)
                       :action ([_] (update-results-graph)))
          :gridx 4 :weightx 1.0
          _ (panel)
          :gridx 0 :gridy 2 :weightx 1.0 :weighty 1.0 :gridwidth 5
          _ graph-panel
          :gridx 0 :gridy 3 :weighty 0.0
          _ (let [p (panel :layout (WrapLayout. FlowLayout/LEFT))]
              (doseq [h (get-headers @problem)]
                (doto p (.add (check-box :caption (name h) :selected false
                                         :action ([_] (toggle-header h))))))
              p)]))
