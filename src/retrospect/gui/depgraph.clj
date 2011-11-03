(ns retrospect.gui.depgraph
  (:import (org.jgraph JGraph))
  (:import (org.jgraph.graph DefaultGraphCell DefaultPort DefaultEdge GraphConstants))
  (:import (com.jgraph.layout JGraphFacade))
  (:import (com.jgraph.layout.organic JGraphFastOrganicLayout))
  (:import (java.awt.image BufferedImage))
  (:import (java.awt Dimension))
  (:import (javax.swing JLabel ImageIcon JViewport))
  (:use [loom.graph :only [edges nodes]])
  (:use [clj-swing.panel])
  (:use [retrospect.state])
  (:use [retrospect.epistemicstates :only [draw-depgraph current-ep-state]]))

(def graph (doto (JGraph.)
             (.setAntiAliased true)))

(def facade (JGraphFacade. graph))

(def layout (JGraphFastOrganicLayout.))

(defn process-depgraph
  [depgraph]
  (when depgraph
    (.remove (.getGraphLayoutCache graph) (.getCells (.getGraphLayoutCache graph)
                                                     true true true true))
    (let [vertex-map (reduce (fn [m n] (let [v (DefaultGraphCell. n)
                                             p (DefaultPort.)]
                                         (GraphConstants/setAutoSize (.getAttributes v) true)
                                         (assoc m n [(doto v (.add p))
                                                     (doto p (.setParent v))])))
                             {} (nodes depgraph))]
      (doseq [a (map first (vals vertex-map))]
        (.insert (.getGraphLayoutCache graph) a))
      (doseq [[a b] (edges depgraph)]
        (let [[vertex-a port-a] (get vertex-map a)
              [vertex-b port-b] (get vertex-map b)
              edge (DefaultEdge.)]
          (GraphConstants/setLineEnd (.getAttributes edge) GraphConstants/ARROW_SIMPLE)
          (.setSource edge port-a)
          (.setTarget edge port-b)
          (.insert (.getGraphLayoutCache graph) edge))))))

(def depgraph-scroll
  (scroll-panel graph))

(defn depgraph-tab
  []
  depgraph-scroll)

(defn update-depgraph
  []
  (let [depgraph (:depgraph (current-ep-state (:ep-state-tree @or-state)))]
    (when (not-empty (edges depgraph))
      (process-depgraph depgraph)
      (.run layout facade)
      (let [nested (.createNestedMap facade true true)]
        (.edit (.getGraphLayoutCache graph) nested))
      (.refresh graph))))
