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
  (:use [retrospect.state]))

(def headers (ref nil))
(def headers-on (ref {}))
(def p-left (panel))
(def p-right (panel))

(def scroll (scroll-panel (JViewport.)))

(defn toggle-header
  [h]
  (dosync (alter headers-on #(assoc % h (not (h %))))))

(defn get-results-viewport
  [or-state]
  (let [hs (filter #(@headers-on %) (sort-by name (keys @headers-on)))
        results-matrix (map (fn [r] (map (fn [h] (h r)) hs))
                            (:results or-state))]
    (doto (JViewport.)
      (.setView  (JTable. (Vector. (map #(Vector. %) results-matrix))
                          (Vector. (sort (map name hs))))))))

(defn update-results
  []
  ;; when possible, update headers
  (when (and (not @headers) (not-empty (:results @or-state)))
    (dosync
     (alter headers (constantly (sort (keys (first (:results @or-state)))))))
    (let [cb (fn [h] (check-box :caption (name h)
                                :selected false
                                :action ([_] (toggle-header h) (update-results))))]
      (doseq [h (take (math/ceil (/ (count @headers) 2)) @headers)]
        (doto p-left (.add (cb h))))
      (doseq [h (take-last (dec (math/ceil (/ (count @headers) 2))) @headers)]
        (doto p-right (.add (cb h))))
      (doto p-right (.add (panel)))))
  (. scroll (setViewport (get-results-viewport @or-state))))

(defn results-tab
  []
  (panel :layout (GridBagLayout.)
         :constrains (java.awt.GridBagConstraints.)
         [:gridx 0 :gridy 0 :weightx 1.0 :weighty 1.0 :gridwidth 5
          :fill :BOTH :insets (Insets. 5 5 5 5)
          _ scroll
          :gridx 0 :gridy 1 :weighty 0.5
          _ (let [p (panel)]
              (.setLayout p (BoxLayout. p BoxLayout/X_AXIS))
              (.setLayout p-left (BoxLayout. p-left BoxLayout/Y_AXIS))
              (.setLayout p-right (BoxLayout. p-right BoxLayout/Y_AXIS))
              (.add p p-left)
              (.add p p-right)
              (scroll-panel p))]))
