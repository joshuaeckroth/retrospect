(ns retrospect.repl
  (:use [retrospect.state])
  (:use [retrospect.player])
  (:use [retrospect.epistemicstates])
  (:use [retrospect.reason.abduction.reason])
  (:use [retrospect.reason.abduction.workspace])
  (:use [retrospect.reason.abduction.meta])
  (:use [retrospect.problems.tracking.problem]))

(defn setup-tracking
  []
  (dosync (alter problem (constantly tracking-problem))
          (alter reasoner (constantly reason-abduction))))

(defn cur-ws
  []
  (:workspace (cur-ep (:est @or-state))))

(defn set-ws
  [ws]
  (dosync (alter or-state
                 assoc :est
                 (update-est (:est @or-state)
                             (assoc (cur-ep (:est @or-state)) :workspace ws))))
  ;; make sure only nil is printed to repl
  nil)
