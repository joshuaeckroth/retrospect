(ns retrospect.reason.abduction.problems.tracking.monitor
  (:use [retrospect.player :only [start-player]])
  (:use [retrospect.state]))

;; The monitor is used to halt the runner and bring up the GUI so that
;; particular situations can be studied.
(defn monitor
  [truedata sensors or-state]
  (if (< 0 (:meta-abductions (:resources or-state)))
    (start-player :monitor true :or-state or-state
                  :sensors sensors :truedata truedata)
    or-state))