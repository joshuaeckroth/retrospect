(ns retrospect.gui.depgraph
  (:import (edu.uci.ics.jung.graph DirectedSparseGraph))
  (:import (edu.uci.ics.jung.algorithms.layout DAGLayout))
  (:import (edu.uci.ics.jung.visualization GraphZoomScrollPane VisualizationViewer))
  (:import (edu.uci.ics.jung.visualization.control DefaultModalGraphMouse))
  (:import (edu.uci.ics.jung.visualization.control ModalGraphMouse$Mode))
  (:import (edu.uci.ics.jung.visualization.decorators ToStringLabeller))
  (:import (java.awt.image BufferedImage))
  (:import (java.awt Dimension))
  (:import (javax.swing JLabel ImageIcon JViewport))
  (:use [loom.graph :only [edges]])
  (:use [clj-swing.panel])
  (:use [retrospect.state])
  (:use [retrospect.epistemicstates :only [draw-depgraph current-ep-state]]))

(def layout (DAGLayout. (DirectedSparseGraph.)))
(def view (doto (VisualizationViewer. layout)
            (.setGraphMouse (doto (DefaultModalGraphMouse.)
                              (.setMode ModalGraphMouse$Mode/TRANSFORMING)))))
(def pane (GraphZoomScrollPane. view))

(defn get-depgraph
  [depgraph]
  (if-not (nil? depgraph)
    (let [g (DirectedSparseGraph.)]
      (doseq [[a b] (edges depgraph)]
        (.addEdge g (format "%s->%s" a b) a b))
      g)))

(defn depgraph-tab
  []
  (.setVertexLabelTransformer (.getRenderContext view) (ToStringLabeller.))
  pane)

(defn update-depgraph
  []
  (let [depgraph (:depgraph (current-ep-state (:ep-state-tree @or-state)))]
    (.setGraphLayout view (doto (DAGLayout. (get-depgraph depgraph))
                            (.setRepulsionRange 500)
                            (.setSize (Dimension. 1000 1000))))
    (.stateChanged view (javax.swing.event.ChangeEvent. (Object.)))))
