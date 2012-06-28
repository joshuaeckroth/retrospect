(ns retrospect.gui.results
  (:import [misc WrapLayout])
  (:import (java.awt GridBagLayout FlowLayout Insets))
  (:import (javax.swing JViewport JTable UIManager BoxLayout RowSorter RowSorter$SortKey SortOrder))
  (:import (javax.swing.table DefaultTableModel TableRowSorter))
  (:import (java.util Vector))
  (:use [clj-swing.panel])
  (:use [clj-swing.label])
  (:use [clj-swing.combo-box])
  (:use [clj-swing.button])
  (:require [clojure.contrib.math :as math])
  (:use [retrospect.state]))

(def headers (ref nil))
(def headers-on (ref {}))
(def sort-key (ref nil))
(def table-model (ref nil))
(def row-sorter (ref nil))
(def p-left (panel))
(def p-middle (panel))
(def p-right (panel))

(def scroll (scroll-panel (JViewport.)))

(defn toggle-header
  [h]
  (dosync (alter headers-on #(assoc % h (not (h %))))))

(defn update-model
  []
  (let [hs (filter #(@headers-on %) (sort-by name (keys @headers-on)))
        results-matrix (map (fn [r] (map (fn [h] (if (= java.lang.Double (type (h r)))
                                             (format "%.2f" (h r))
                                             (h r)))
                                    hs))
                          @results)]
    (dosync
     (alter table-model (constantly (DefaultTableModel.(Vector. (map #(Vector. %) results-matrix))
                                      (Vector. (sort (map name hs))))))
     (alter row-sorter (constantly (TableRowSorter. @table-model))))))

(defn get-results-viewport
  []
  (if @sort-key
    (.setSortKeys @row-sorter [@sort-key])
    (.setSortKeys @row-sorter []))
  (doto (JViewport.)
    (.setView (doto (JTable. @table-model)
                (.setRowSorter @row-sorter)))))

(defn set-sorted
  [h]
  (if (get @headers-on h)
    (let [i (first (filter #(= (name h) (.getColumnName @table-model %))
                      (range (.getColumnCount @table-model))))
          order SortOrder/DESCENDING]
      (dosync (alter sort-key (constantly (RowSorter$SortKey. i order))))
      (.setSortKeys @row-sorter [@sort-key]))
    (dosync (alter sort-key (constantly nil)))))

(defn update-results
  []
  ;; when possible, update headers
  (when (and (not @headers) (not-empty @results))
    (dosync
     (alter headers (constantly (sort (keys (first @results))))))
    (let [cb (fn [h] (check-box :caption (name h)
                               :selected false
                               :action ([_] (toggle-header h) (update-model)
                                          (set-sorted h) (update-results))))
          groups (partition-all (Math/ceil (/ (count @headers) 3)) @headers)]
      (doseq [h (nth groups 0)]
        (doto p-left (.add (cb h))))
      (doseq [h (nth groups 1)]
        (doto p-middle (.add (cb h))))
      (doseq [h (nth groups 2)]
        (doto p-right (.add (cb h))))))
  (when (nil? @table-model)
    (update-model))
  (. scroll (setViewport (get-results-viewport))))

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
    (.setDividerLocation 300)))
