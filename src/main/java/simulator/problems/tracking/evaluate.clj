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

(defn evaluate
  [ep-state sensors truedata params]
  "The current ep-state has accepted the decision of the previous ep-state;
   however, the current ep-state has a time 1+ the previous, in which the
   events occurred and were explained; thus, we must get the 'truedata' from
   the current ep-state's time minus 1."
  (let [trueeventlog (:eventlog (get truedata (dec (:time ep-state))))
        pdata (:problem-data ep-state)
        pevents (set (get-events pdata))
        pentities (get-entities pdata)
        trueevents (set (get-events trueeventlog))
        trueentities (get-entities trueeventlog)
        events-correct (count (set/intersection trueevents pevents))
        events-wrong (count (set/difference pevents trueevents))
	events-total (count trueevents)
        identities-correct (count (find-correct-identities trueentities pentities))
        identities-total (count trueentities)]
    {:PercentEventsCorrect
     (double (* 100 (/ events-correct events-total)))
     :PercentEventsWrong
     (double (* 100 (/ events-wrong events-total)))
     :PercentIdentitiesCorrect
     (double (* 100 (/ identities-correct identities-total)))
     :AvgWalk (calc-average-walk trueentities)
     :SensorCoverage (measure-sensor-coverage
                      (:GridWidth params) (:GridHeight params) sensors)
     :SensorOverlap (measure-sensor-overlap
                     (:GridWidth params) (:GridHeight params) sensors)}))
