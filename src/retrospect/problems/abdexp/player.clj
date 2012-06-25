(ns retrospect.problems.abdexp.player
  (:import (javax.swing JTabbedPane))
  (:use [loom.graph :only [digraph]])
  (:use [clj-swing.panel])
  (:use [retrospect.gui.graphs])
  (:use [retrospect.problems.abdexp.expgraph :only [format-dot-expgraph]])
  (:use [retrospect.epistemicstates :only [cur-ep]])
  (:use [retrospect.state]))

(def canvases (ref {:initial nil
                    :arbitrary nil
                    :efli nil}))

(def current-hypgraph-dot (ref ""))
(def current-hypgraph-svg (ref ""))

(defn listener
  [node])

(defn player-get-stats-panel
  []
  (panel))

  (comment (when (< 0 @time-now)
             (let [{:keys [arb efli]} (:workspace (cur-ep (:est @or-state)))]
               (generate-graph (format-dot-expgraph (nth (:test @truedata) (dec @time-now)))
                               (:initial @canvases) listener false)
               (when arb
                 (generate-graph (format-dot-expgraph arb)
                                 (:arbitrary @canvases) listener false))
               (when efli
                 (generate-graph (format-dot-expgraph efli)
                                 (:efli @canvases) listener false)))))

(defn player-update-diagram
  []
  (if (< 0 @time-now)
    (generate-graph (format-dot-expgraph (get (:test @truedata) @time-now))
                    (:initial @canvases) listener false
                    current-hypgraph-dot current-hypgraph-svg)
    (generate-graph (digraph) (:initial @canvases) listener false
                    current-hypgraph-dot current-hypgraph-svg)))

(defn player-setup-diagram
  []
  (dosync (alter canvases (constantly {:initial (create-canvas)
                                       :arbitrary (create-canvas)
                                       :efli (create-canvas)})))
  (doto (JTabbedPane.)
    (.addTab "Initial" (:initial @canvases))
    (.addTab "Arbitrary" (:arbitrary @canvases))
    (.addTab "EFLI" (:efli @canvases))))
