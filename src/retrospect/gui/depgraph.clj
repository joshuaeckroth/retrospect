(ns retrospect.gui.depgraph
  (:import (java.io StringReader))
  (:import (java.awt GridBagLayout Insets Graphics Dimension Color))
  (:import (java.awt.image BufferedImage))
  (:import (javax.swing JLabel ImageIcon JViewport))
  (:import (org.apache.batik.dom.svg SAXSVGDocumentFactory))
  (:import (org.apache.batik.swing JSVGCanvas))
  (:import (org.apache.batik.util XMLResourceDescriptor))
  (:use [loom.graph :only [edges nodes]])
  (:use [loom.io :only [dot-str]])
  (:use [loom.attr :only [add-attr]])
  (:use [clojure.java.shell :only [sh]])
  (:use [clj-swing.core :only [add-action-listener]])
  (:use [clj-swing.panel])
  (:use [clj-swing.button])
  (:use [retrospect.state])
  (:use [retrospect.epistemicstates :only [current-ep-state]]))

(def canvas (JSVGCanvas.))

(defn generate-depgraph
  []
  (let [depgraph (:depgraph (current-ep-state (:ep-state-tree @or-state)))]
    (if (and depgraph (not-empty (edges depgraph)))
      (let [dot (dot-str (reduce (fn [g n]
                                   (-> g (add-attr n :label (:id n))
                                       (add-attr n :id (:id n))))
                                 depgraph (nodes depgraph))
                         :graph {:dpi 60 :rankdir "LR"})
            {svg :out} (sh "dot" "-Tsvg" :in dot)
            sr (StringReader. svg)
            parser (XMLResourceDescriptor/getXMLParserClassName)
            doc (try (.createDocument (SAXSVGDocumentFactory. parser)
                                      "file:///depgraph" sr)
                     (catch Exception e (println e)))]
        (.setDocumentState canvas JSVGCanvas/ALWAYS_DYNAMIC)
        (.setDocument canvas doc)))))

(defn depgraph-tab
  []
  (panel :layout (GridBagLayout.)
         :constrains (java.awt.GridBagConstraints.)
         [:gridx 0 :gridy 0 :weightx 1.0 :weighty 1.0 :gridwidth 2 :fill :BOTH
          :insets (Insets. 5 5 5 5)
          _ (scroll-panel canvas)
          :gridy 1 :gridwidth 1 :gridx 0 :weightx 1.0 :weighty 0.0
          _ (panel)
          :gridx 1 :weightx 0.0
          _ (doto (button "Generate")
              (add-action-listener ([_] (generate-depgraph))))]))

(defn update-depgraph
  [])
