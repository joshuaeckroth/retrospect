(ns retrospect.problems.tracking.monitor
  (:use [retrospect.player :only [start-player]])
  (:use [retrospect.epistemicstates :only [previous-ep-state]]))

;; The monitor is used to halt the runner and bring up the GUI so that
;; particular situations can be studied.
(defn monitor
  [problem truedata sensors or-state params]
  or-state)