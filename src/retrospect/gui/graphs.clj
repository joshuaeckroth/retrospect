(ns retrospect.gui.graphs
  (:import (java.io StringReader))
  (:import (org.apache.batik.dom.svg SAXSVGDocumentFactory))
  (:import (org.apache.batik.swing JSVGCanvas))
  (:import (org.apache.batik.swing.gvt Interactor))
  (:import (org.apache.batik.util XMLResourceDescriptor))
  (:import (org.w3c.dom.events EventListener))
  (:use [loom.graph :only [edges nodes]])
  (:use [loom.io :only [dot-str]])
  (:use [loom.attr :only [add-attr]])
  (:use [clojure.java.shell :only [sh]]))

(defn node-listener
  [f]
  (proxy [EventListener] []
    (handleEvent [e] (let [attrs (.getAttributes (.getParentNode (.getTarget e)))
                           id (.getNodeValue (.getNamedItem attrs "id"))]
                       (f id)))))

(defn create-canvas
  []
  (JSVGCanvas.))

(defn generate-graph
  [graph canvas listener left-to-right?]
  (if (and graph (not-empty (edges graph)))
    (let [dot (dot-str (reduce (fn [g n]
                                 (-> g (add-attr n :label (:id n))
                                     (add-attr n :id (:id n))))
                               graph (nodes graph))
                       :graph {:dpi 60 :rankdir (if left-to-right? "LR" "")})
          {svg :out} (sh "dot" "-Tsvg" :in dot)
          sr (StringReader. svg)
          parser (XMLResourceDescriptor/getXMLParserClassName)
          doc (try (.createDocument (SAXSVGDocumentFactory. parser)
                                    "file:///graph" sr)
                   (catch Exception e (println e)))]
      (doto canvas
        (.setDocumentState JSVGCanvas/ALWAYS_DYNAMIC)
        (.setDocument doc))
      (let [svgdoc (.getSVGDocument canvas)
            nodes (.getElementsByTagName svgdoc "g")]
        (doseq [i (range (.getLength nodes))]
          (.addEventListener (.item nodes i) "click" (node-listener listener) false))))))

