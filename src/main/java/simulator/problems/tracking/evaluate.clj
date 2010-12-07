(ns simulator.problems.tracking.evaluate
  (:use [simulator.problems.tracking.positions :only (manhattan-distance)])
  (:use [simulator.problems.tracking.entities :only (pair-snapshots)])
  (:use [simulator.problems.tracking.eventlog :only (get-entities get-events)])
  (:use [simulator.epistemicstates :only (current-ep-state)])
  (:use [simulator.problems.tracking.sensors :only
         (measure-sensor-overlap measure-sensor-coverage)])
  (:use [simulator.problems.tracking.hypotheses :only [update-problem-data]])
  (:require [clojure.set :as set]))

(defn calc-average-walk
  [eventlog]
  (let [calc-walk-sum
	(fn [sum [es-old es-new]]
	  (+ sum (manhattan-distance (:pos es-old) (:pos es-new))))

	sum-walks
	(fn [pairs] (reduce calc-walk-sum 0 pairs))

	walk-avgs
	(doall (map (fn [e] (let [pairs (pair-snapshots e)]
                              (if (empty? pairs) 0 (/ (sum-walks pairs) (count pairs)))))
                    (get-entities eventlog)))]
    (double (/ (reduce + 0 walk-avgs) (count walk-avgs)))))

(defn find-correct-identities
  [eventlog pdata]
  "An 'identity' of an entity is its starting position (at a specific
   time). Here we compare starting positions with ending positions to
   see if the believed entities can also match start-end
   positions. This means a believed 'identity' is correct even when
   the inner path is wrong; only the beginning and end must match up
   with the true entities."
  (let [get-starts-ends (fn [es] (set (doall (map (fn [e] {:start (first (:snapshots e))
                                                           :end (last (:snapshots e))})
                                                  es))))
        true-starts-ends (get-starts-ends (get-entities eventlog))
        strat-starts-ends (get-starts-ends (get-entities pdata))]
    (set/intersection true-starts-ends strat-starts-ends)))

(defn evaluate
  [ep-state sensors truedata params]
  (let [trueevents (:eventlog (get truedata (:time ep-state)))
        pdata (:problem-data (update-problem-data ep-state true))
        events-correct (count (set/intersection (set (get-events trueevents))
                                            (set (get-events pdata))))
	events-total (count (get-events trueevents))
        identities-correct (count (find-correct-identities
                                   trueevents pdata))
        identities-total (count (get-entities trueevents))]
    {:PercentEventsCorrect
     (double (* 100 (/ events-correct events-total)))
     :PercentIdentitiesCorrect
     (double (* 100 (/ identities-correct identities-total)))
     :AvgWalk (calc-average-walk trueevents)
     :SensorCoverage (measure-sensor-coverage
                      (:GridWidth params) (:GridHeight params) sensors)
     :SensorOverlap (measure-sensor-overlap
                     (:GridWidth params) (:GridHeight params) sensors)}))
