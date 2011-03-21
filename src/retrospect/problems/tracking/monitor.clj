(ns retrospect.problems.tracking.monitor
  (:use [retrospect.player :only [start-player]])
  (:use [retrospect.epistemicstates :only [previous-ep-state]]))

;; The monitor is used to halt the runner and bring up the GUI so that
;; particular situations can be studied.
(defn monitor
  [problem truedata sensors or-state params]
  (if (< 0 (:meta-abductions (:resources or-state)))
    (start-player problem :monitor true :or-state or-state
                  :params params :sensors sensors :truedata truedata)
    or-state))