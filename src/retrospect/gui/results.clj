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
(def p-middle (panel))
(def p-right (panel))

(def scroll (scroll-panel (JViewport.)))

(defn toggle-header
  [h]
  (dosync (alter headers-on #(assoc % h (not (h %))))))

(defn get-results-viewport
  [or-state]
  (let [hs (filter #(@headers-on %) (sort-by name (keys @headers-on)))
        results-matrix (map (fn [r] (map (fn [h] (if (= java.lang.Double (type (h r)))
                                                   (format "%.2f" (h r))
                                                   (h r)))
                                         hs))
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
                                :action ([_] (toggle-header h) (update-results))))
          groups (partition-all (Math/ceil (/ (count @headers) 3)) @headers)]
      (doseq [h (nth groups 0)]
        (doto p-left (.add (cb h))))
      (doseq [h (nth groups 1)]
        (doto p-middle (.add (cb h))))
      (doseq [h (nth groups 2)]
        (doto p-right (.add (cb h))))))
  (. scroll (setViewport (get-results-viewport @or-state))))

(defn results-tab
  []
  (doto (split-vertical
         scroll
         (let [p (panel)]
           (.setLayout p (BoxLayout. p BoxLayout/X_AXIS))
           (.setLayout p-left (BoxLayout. p-left BoxLayout/Y_AXIS))
           (.setLayout p-middle (BoxLayout. p-middle BoxLayout/Y_AXIS))
           (.setLayout p-right (BoxLayout. p-right BoxLayout/Y_AXIS))
           (.add p p-left)
           (.add p p-middle)
           (.add p p-right)
           (scroll-panel p)))
    (.setDividerLocation 500)))
