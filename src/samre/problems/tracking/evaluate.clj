(ns samre.problems.tracking.evaluate
  (:use [samre.problems.tracking.positions :only (manhattan-distance)])
  (:use [samre.problems.tracking.entities :only (pair-snapshots)])
  (:use [samre.problems.tracking.eventlog :only (get-entities get-events)])
  (:use [samre.epistemicstates :only (current-ep-state)])
  (:use [samre.confidences])
  (:require [clojure.set :as set]))

(defn calc-average-walk
  [entities]
  (let [calc-walk-sum
	(fn [sum [es-old es-new]]
	  (+ sum (manhattan-distance (:pos es-old) (:pos es-new))))

	sum-walks
	(fn [pairs] (reduce calc-walk-sum 0 pairs))

	walk-avgs
	(doall (map (fn [e] (let [pairs (pair-snapshots e)]
                              (if (empty? pairs) 0 (/ (sum-walks pairs) (count pairs)))))
                    entities))]
    (double (/ (reduce + 0 walk-avgs) (count walk-avgs)))))

(defn find-correct-identities
  [trueentities pentities]
  "An 'identity' of an entity is its starting position (at a specific
   time). Here we compare starting positions with ending positions to
   see if the believed entities can also match start-end
   positions. This means a believed 'identity' is correct even when
   the inner path is wrong; only the beginning and end must match up
   with the true entities."
  (let [get-starts-ends (fn [es] (set (doall (map (fn [e] {:start (first (:snapshots e))
                                                           :end (last (:snapshots e))})
                                                  es))))
        true-starts-ends (get-starts-ends trueentities)
        strat-starts-ends (get-starts-ends pentities)]
    (set/intersection true-starts-ends strat-starts-ends)))

;; TODO: handle disappear events properly
(defn measure-plausibility-accuracy
  [workspace trueevents]
  (let [accepted (doall (filter #(= (:type %) :tracking) (:accepted workspace)))
        correct-event (fn [h] (if (or (= (type (:event (:data h)))
                                         samre.problems.tracking.events.EventFrozen)
                                      (= (type (:event (:data h)))
                                         samre.problems.tracking.events.EventDisappear))
                                false
                                (some #(= % (:event (:data h))) trueevents)))
        correct-frozen (fn [h] (if (not (or (= (type (:event (:data h)))
                                               samre.problems.tracking.events.EventFrozen)
                                            (= (type (:event (:data h)))
                                               samre.problems.tracking.events.EventDisappear)))
                                 false
                                 (not-any? (fn [e] (and (= (:time e)
                                                           (:time (:event (:data h))))
                                                        (or (= (:oldpos e)
                                                               (:pos (:event (:data h))))
                                                            (= (:pos e)
                                                               (:pos (:event (:data h)))))))
                                           trueevents)))
        hyps-correct (doall (filter (fn [h] (or (correct-event h) (correct-frozen h)))
                                    accepted))
        hyps-wrong (set/difference (set accepted) (set hyps-correct))
        value (fn [h op] (cond (op NEUTRAL (:confidence h)) 1
                               (= NEUTRAL (:confidence h)) 0
                               :else -1))
        positive (reduce + 0 (map #(value % <) hyps-correct))
        negative (reduce + 0 (map #(value % >) hyps-wrong))]
    (if (empty? (concat hyps-correct hyps-wrong)) 0.0
        (double (/ (+ positive negative) (count (concat hyps-correct hyps-wrong)))))))

(defn event-seen
  [event sensors-seen]
  (and (some #(= % (:pos event)) sensors-seen)
       (if (:oldpos event) (some #(= % (:oldpos event)) sensors-seen) true)))

(defn evaluate
  [ep-state sensors truedata params]
  "The current ep-state has accepted the decision of the previous ep-state;
   however, the current ep-state has a time 1+ the previous, in which the
   events occurred and were explained; thus, we must get the 'truedata' from
   the current ep-state's time minus 1."
  (let [trueeventlog (:eventlog (get truedata (dec (:time ep-state))))
        pdata (:problem-data ep-state)
        ;; don't penalize (as "wrong") frozen and disappear events
        pevents (set (filter
                      #(and (not= samre.problems.tracking.events.EventFrozen (type %))
                            (not= samre.problems.tracking.events.EventDisappear (type %)))
                      (get-events (:eventlog pdata))))
        pentities (get-entities (:eventlog pdata))
        trueevents (set (filter #(event-seen % (:sensors-seen (:problem-data ep-state)))
                                (get-events trueeventlog)))
        trueentities (get-entities trueeventlog)
        events-correct (count (set/intersection trueevents pevents))
        events-wrong (count (set/difference pevents trueevents))
	events-total (count trueevents)
        identities-correct (count (find-correct-identities trueentities pentities))
        identities-total (count trueentities)]
    {:PercentEventsCorrect
     (if (= 0 events-total) 100.0
         (double (* 100 (/ events-correct events-total))))
     :PercentEventsWrong
     (if (= 0 events-total) 0.0
         (double (* 100 (/ events-wrong events-total))))
     :PercentIdentitiesCorrect
     (double (* 100 (/ identities-correct identities-total)))
     :AvgWalk (calc-average-walk trueentities)
     :PlausibilityAccuracy (measure-plausibility-accuracy (:workspace ep-state) trueevents)
     :SensorCoverage (:sensor-coverage pdata)
     :SensorOverlap (:sensor-overlap pdata)}))
