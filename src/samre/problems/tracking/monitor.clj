(ns samre.problems.tracking.monitor
  (:use [samre.player.gui :only [start-player]])
  (:use [samre.epistemicstates :only [previous-ep-state]])
  (:use [samre.workspaces :only [lookup-hyps]]))

;; The monitor is used to halt the runner and bring up the GUI so that
;; particular situations can be studied.

(defn monitor
  [problem truedata sensors or-state params]
  (if-let [ep (previous-ep-state (:ep-state-tree or-state))]
    (let [workspace (:workspace ep)
          explains-nothing? (fn [hyp] (and (empty? (filter #((:hyps workspace) %)
                                                           (:explains hyp)))
                                           (not-any? #(= % (:id hyp)) (:accepted workspace))))
          hyps (filter #(= (:type %) :tracking) (vals (:hyps workspace)))]
      (if (some explains-nothing? hyps)
        (start-player problem :monitor true :or-state or-state
                      :params params :sensors sensors :truedata truedata)
        or-state))
    or-state))