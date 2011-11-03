(ns retrospect.gui.depgraph
  (:import (com.mxgraph.view mxGraph))
  (:import (com.mxgraph.swing mxGraphComponent))
  (:import (com.mxgraph.model mxCell))
  (:import (com.mxgraph.layout mxOrganicLayout))
  (:import (java.awt.image BufferedImage))
  (:import (java.awt Dimension Rectangle))
  (:import (javax.swing JLabel ImageIcon JViewport))
  (:use [loom.graph :only [edges nodes]])
  (:use [clj-swing.panel])
  (:use [retrospect.state])
  (:use [retrospect.epistemicstates :only [draw-depgraph current-ep-state]]))

(def graph (doto (mxGraph.)
             (.setAutoSizeCells true)
             (.setCellsDisconnectable false)))

(def layout (doto (mxOrganicLayout. graph)
              (.setFineTuning true)
              (.setOptimizeEdgeCrossing true)
              (.setOptimizeEdgeDistance true)
              (.setOptimizeEdgeLength true)
              (.setOptimizeNodeDistribution true)
              (.setOptimizeBorderLine true)))

(defn process-depgraph
  [depgraph]
  (when depgraph
    (let [parent (.getDefaultParent graph)
          vertex-map (reduce (fn [m n] (assoc m n (.insertVertex graph parent nil n 0 0 0 0)))
                             {} (nodes depgraph))]
      (doseq [[a b] (edges depgraph)]
        (let [va (get vertex-map a)
              vb (get vertex-map b)]
          (.insertEdge graph parent nil nil va vb))))))

(defn depgraph-tab
  []
  (mxGraphComponent. graph))

(defn update-depgraph
  []
  (let [depgraph (:depgraph (current-ep-state (:ep-state-tree @or-state)))]
    (when (not-empty (edges depgraph))
      (.beginUpdate (.getModel graph))
      (.removeCells graph (.getChildCells graph (.getDefaultParent graph)))
      (process-depgraph depgraph)
      (.endUpdate (.getModel graph))
      (.execute layout (.getDefaultParent graph)))))
