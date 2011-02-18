(ns samre.problems.tracking.monitor
  (:use [samre.player.gui :only [start-player]])
  (:use [samre.epistemicstates :only [previous-ep-state]])
  (:use [samre.workspaces :only [lookup-hyps]]))

;; The monitor is used to halt the runner and bring up the GUI so that
;; particular situations can be studied.

(defn monitor
  [problem truedata sensors or-state params]
  or-state)