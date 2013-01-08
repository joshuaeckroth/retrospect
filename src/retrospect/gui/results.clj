(ns retrospect.gui.results
  (:import (javax.swing BoxLayout))
  (:use [seesaw.core])
  (:use [seesaw.table])
  (:use [seesaw.swingx])
  (:use [clj-swing.panel :only [panel split-vertical scroll-panel]])
  (:use [clj-swing.button :only [check-box]])
  (:use [retrospect.state]))

(def headers (ref nil))
(def headers-on (ref {}))

(def p-left (panel))
(def p-right (panel))

(def results-table-model (ref (table-model :columns [])))

(def results-table
  (table-x :model @results-table-model
           :column-control-visible? false
           :horizontal-scroll-enabled? true))

(defn toggle-header
  [h]
  (dosync (alter headers-on #(assoc % h (not (h %))))))

(defn update-results
  []
  ;; when possible, update headers
  (when (and (not @headers) (not-empty @results))
    (dosync (alter headers (constantly (sort (keys (first @results))))))
    (let [cb (fn [h] (check-box :caption (name h)
                               :selected false
                               :action ([_] (toggle-header h) (update-results))))
          groups (partition-all (Math/ceil (/ (count @headers) 2)) @headers)]
      (doseq [h (nth groups 0)]
        (doto p-left (.add (cb h))))
      (doseq [h (nth groups 1)]
        (doto p-right (.add (cb h))))))
  (println @headers-on)
  (dosync (alter results-table-model
                 (constantly (table-model :columns
                                          (sort-by name (filter #(@headers-on %)
                                                           (keys @headers-on)))))))
  (.setModel results-table @results-table-model)
  (doseq [i (range (count @results))]
    (insert-at! @results-table-model i (select-keys (nth @results i)
                                                    (filter #(@headers-on %)
                                                       (keys @headers-on))))))

(defn results-tab
  []
  (doto (split-vertical
         (scrollable results-table)
         (let [p (panel)]
           (.setLayout p (BoxLayout. p BoxLayout/X_AXIS))
           (.setLayout p-left (BoxLayout. p-left BoxLayout/Y_AXIS))
           (.setLayout p-right (BoxLayout. p-right BoxLayout/Y_AXIS))
           (.add p p-left)
           (.add p p-right)
           (scroll-panel p)))
    (.setDividerLocation 300)))
