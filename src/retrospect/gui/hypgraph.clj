(ns retrospect.gui.hypgraph
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
  (:use [retrospect.epistemicstates :only [previous-ep-state]]))

(def canvas (JSVGCanvas.))

(defn generate-hypgraph
  []
  (let [hypgraph (:graph-static (:workspace (previous-ep-state (:ep-state-tree @or-state))))]
    (if (and hypgraph (not-empty (edges hypgraph)))
      (let [dot (dot-str (reduce (fn [g n]
                                   (-> g (add-attr n :label (:id n))
                                       (add-attr n :id (:id n))))
                                 hypgraph (nodes hypgraph))
                         :graph {:dpi 60 :rankdir "LR"})
            {svg :out} (sh "dot" "-Tsvg" :in dot)
            sr (StringReader. svg)
            parser (XMLResourceDescriptor/getXMLParserClassName)
            doc (try (.createDocument (SAXSVGDocumentFactory. parser)
                                      "file:///hypgraph" sr)
                     (catch Exception e (println e)))]
        (.setDocumentState canvas JSVGCanvas/ALWAYS_DYNAMIC)
        (.setDocument canvas doc)))))

(defn hypgraph-tab
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
              (add-action-listener ([_] (generate-hypgraph))))]))

(defn update-hypgraph
  [])
