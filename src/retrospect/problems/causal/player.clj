(ns retrospect.problems.causal.player
  (:import (org.apache.batik.swing JSVGCanvas))
  (:use [loom.graph :only [nodes]])
  (:use [loom.attr :only [attr add-attr]])
  (:use [clj-swing.panel])
  (:use [retrospect.gui.graphs])
  (:use [retrospect.state]))

(def canvas (JSVGCanvas.))

(defn update-attrs
  [network]
  (reduce (fn [g node] (cond (= :on (attr g node :value))
                             (add-attr g node :color "green")
                             (= :off (attr g node :value))
                             (add-attr g node :color "red")
                             :else g))
          network (nodes network)))

(defn player-setup-diagram
  []
  (let [network (update-attrs (:network @truedata))]
    (generate-graph network canvas))
  canvas)

(defn player-update-diagram
  []
  (let [network (update-attrs (:network @truedata))]
    (generate-graph network canvas)))

(defn player-get-stats-panel
  []
  (panel))

(defn player-update-stats
  [])

(defn player-get-truedata-log
  []
  (if (>= @time-now (count (:observed-seq @truedata))) ""
      (str (nth (:observed-seq @truedata) @time-now))))

(defn player-get-problem-log
  []
  (let [pdata (:problem-data (:ep-state @or-state))]
    (str (:believed pdata))))
