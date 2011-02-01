(ns samre.problems.tracking.monitor
  (:use [samre.player.gui :only [start-player]])
  (:use [samre.problems.tracking.eventlog :only [get-events]])
  (:use [samre.problems.tracking.evaluate :only [interesting-event event-seen]])
  (:use [samre.epistemicstates :only [previous-ep-state]])
  (:use [samre.workspaces :only [lookup-hyps]]))

;; The monitor is used to halt the runner and bring up the GUI so that
;; particular situations can be studied.


(defn wrong-choice
  "Detect the following situation: a wrong event hyp was accepted, and
  it conflicts with another hyp that is correct, and the correct hyp
  had a greater apriori value than the wrong, accepted hyp."
  [truedata ep-state]
  (let [ws (:workspace ep-state)
        accepted (filter #(and (= (:type %) :tracking)
                               (interesting-event (:event (:data %))))
                         (lookup-hyps ws (:accepted (:decision ws))))
        trueevents (filter #(event-seen % (:sensors-seen (:problem-data ep-state)))
                           (get-events (:eventlog (get truedata (:time ep-state)))))
        wrong (filter (fn [h] (not-any? #(= (:event (:data h)) %) trueevents)) accepted)
        wrong-choices
        (filter identity
                (map (fn [h] (let [im (filter #(< (:apriori h) (:apriori %))
                                              ((:impossible-fn h) h
                                               (vals (:hyps ws))))
                                   better (filter (fn [h]
                                                    (some #(= (:event (:data h)) %)
                                                          trueevents)) im)]
                               (when (not-empty better)
                                 {:wrong h :better better})))
                     wrong))]
    (when (< 0 (count wrong-choices))
      (doseq [w wrong-choices]
        (println (format "%s: %s" (name (:id (:wrong w)))
                         (apply str (interpose "," (map (comp name :id) (:better w))))))))
    wrong-choices))

(defn monitor
  [problem truedata sensors or-state params]
  (let [prev-ep (previous-ep-state (:ep-state-tree or-state))]
    (if (nil? prev-ep) or-state
      (if (< 0 (count (wrong-choice truedata prev-ep)))
        (start-player problem :monitor true :or-state or-state
                      :params params :sensors sensors :truedata truedata)
        or-state))))