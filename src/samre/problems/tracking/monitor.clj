(ns samre.problems.tracking.monitor
  (:use [samre.player.gui :only [start-player]]))

(defn monitor
  [problem truedata sensors or-state params]
  (if (> (rand) 0.8)
    (start-player problem :monitor true :or-state or-state
                  :params params :sensors sensors :truedata truedata)
    or-state))