(ns retrospect.gui.results
  (:import [misc WrapLayout])
  (:import (java.awt GridBagLayout FlowLayout Insets))
  (:import (javax.swing JViewport JTable UIManager))
  (:import (java.util Vector))
  (:use [clj-swing.panel])
  (:use [clj-swing.label])
  (:use [clj-swing.combo-box])
  (:use [clj-swing.button])
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
                  p (panel :layout (WrapLayout.))]
              (doseq [h (sort-by name (get-headers @problem))]
                (doto p (.add (cb h))))
              (scroll-panel p))]))
