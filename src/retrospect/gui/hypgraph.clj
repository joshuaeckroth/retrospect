(ns retrospect.gui.hypgraph
  (:import (java.io StringReader))
  (:import (java.awt GridBagLayout Insets Graphics Dimension Color))
  (:import (java.awt.image BufferedImage))
  (:import (javax.swing JLabel ImageIcon JViewport))
  (:import (org.apache.batik.dom.svg SAXSVGDocumentFactory))
  (:import (org.apache.batik.swing JSVGCanvas))
  (:import (org.apache.batik.util XMLResourceDescriptor))
  (:use [loom.graph :only [edges]])
  (:use [loom.io :only [dot-str]])
  (:use [clojure.java.shell :only [sh]])
  (:use [clj-swing.core :only [add-action-listener]])
  (:use [clj-swing.panel])
  (:use [clj-swing.button])
  (:use [retrospect.state])
  (:use [retrospect.epistemicstates :only
         [current-ep-state previous-ep-state]]))

(def canvas (JSVGCanvas.))

(defn generate-hypgraph
  []
  (let [ep-state (let [ep (current-ep-state (:ep-state-tree @or-state))]
                   (if (re-find #"\?" (str ep))
                     (previous-ep-state (:ep-state-tree @or-state)) ep))
        hypgraph (:graph-static (:workspace ep-state))
        dot (dot-str hypgraph :graph {:dpi 60 :rankdir "LR"})
        {svg :out} (sh "dot" "-Tsvg" :in dot)
        sr (StringReader. svg)
        parser (XMLResourceDescriptor/getXMLParserClassName)
        doc (try (.createDocument (SAXSVGDocumentFactory. parser)
                                  "file:///hypgraph" sr)
                 (catch Exception e (println e)))]
    (.setDocumentState canvas JSVGCanvas/ALWAYS_DYNAMIC)
    (.setDocument canvas doc)))

(defn hypgraph-tab
  []
  (panel :layout (GridBagLayout.)
         :constrains (java.awt.GridBagConstraints.)
         [:gridx 0 :gridy 0 :weightx 1.0 :weighty 1.0 :gridwidth 2 :fill :BOTH
          :insets (Insets. 5 5 5 5)
          _ canvas
          :gridy 1 :gridwidth 1 :gridx 0 :weightx 1.0 :weighty 0.0
          _ (panel)
          :gridx 1 :weightx 0.0
          _ (doto (button "Generate")
              (add-action-listener ([_] (generate-hypgraph))))]))

(defn update-hypgraph
  [])
