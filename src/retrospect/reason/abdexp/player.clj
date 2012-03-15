(ns retrospect.reason.abdexp.player
  (:import (javax.swing JTabbedPane))
  (:use [clj-swing.panel])
  (:use [retrospect.gui.graphs])
  (:use [retrospect.problems.abdexp.expgraph :only [format-dot-expgraph]])
  (:use [retrospect.epistemicstates :only [cur-ep]])
  (:use [retrospect.state]))

(def canvases (ref {:initial nil
                    :arbitrary nil
                    :efli nil}))

(defn listener
  [node])

(defn player-get-stats-panel
  []
  (panel))

(defn player-update-diagram
  []
  (let [{:keys [arb efli]} (:workspace (cur-ep (:est @or-state)))]
    (generate-graph (format-dot-expgraph (:test @truedata))
                    (:initial @canvases) listener false)
    (when arb
      (generate-graph (format-dot-expgraph arb)
                      (:arbitrary @canvases) listener false))
    (when efli
      (generate-graph (format-dot-expgraph efli)
                      (:efli @canvases) listener false))))

(defn player-setup-diagram
  []
  (dosync (alter canvases (constantly {:initial (create-canvas)
                                       :arbitrary (create-canvas)
                                       :efli (create-canvas)})))
  (doto (JTabbedPane.)
    (.addTab "Initial" (:initial @canvases))
    (.addTab "Arbitrary" (:arbitrary @canvases))
    (.addTab "EFLI" (:efli @canvases))))
