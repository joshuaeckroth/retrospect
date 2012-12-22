(ns retrospect.gui.graphs
  (:import (java.io StringReader))
  (:import (javax.swing JFileChooser))
  (:import (javax.swing.filechooser FileNameExtensionFilter))
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
  [graph canvas listener left-to-right? dot-ref svg-ref]
  (if (and graph (or (string? graph) (not-empty (edges graph))))
    (let [dot (if (string? graph) graph
                  (dot-str graph :graph {:dpi 60 :rankdir (if left-to-right? "LR" "")}
                           :node {:shape "plaintext" :fontname "sans"}))
          {svg :out} (sh "dot" "-Tsvg" :in dot)
          sr (StringReader. svg)
          parser (XMLResourceDescriptor/getXMLParserClassName)
          doc (try (.createDocument (SAXSVGDocumentFactory. parser)
                                    "file:///graph" sr)
                   (catch Exception e (do (println e))))]
      (when doc
        (dosync (alter dot-ref (constantly dot)))
        (dosync (alter svg-ref (constantly svg)))
        (doto canvas
          (.setDocumentState JSVGCanvas/ALWAYS_DYNAMIC)
          (.setDocument doc))
        (let [nodes (.getElementsByTagName doc "g")]
          (doseq [i (range (.getLength nodes))]
            (.addEventListener (.item nodes i) "click" (node-listener listener) false)))))))

(defn save-dot
  [dot]
  (let [chooser (JFileChooser.)
        filter (FileNameExtensionFilter. "Dot files" (into-array String ["dot"]))]
    (.setFileFilter chooser filter)
    (when (= JFileChooser/APPROVE_OPTION (.showSaveDialog chooser nil))
      (let [filename (.. chooser (getSelectedFile) (getAbsolutePath))]
        (spit filename dot)))))

(defn save-svg
  [svg]
  (let [chooser (JFileChooser.)
        filter (FileNameExtensionFilter. "SVG documents" (into-array String ["svg"]))]
    (.setFileFilter chooser filter)
    (when (= JFileChooser/APPROVE_OPTION (.showSaveDialog chooser nil))
      (let [filename (.. chooser (getSelectedFile) (getAbsolutePath))]
        (spit filename svg)))))


