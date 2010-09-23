(ns simulator.problems.tracking.core
  (:require [simulator.problems.tracking events grid entities])
  (:import [simulator.problems.tracking.events EventNew EventMove])
  (:import [simulator.problems.tracking.entities Entity EntitySnapshot])
  (:import [simulator.problems.tracking.grid GridState])
  (:use clojure.set)
  (:use [simulator.problems.tracking.entities :only (pos pair-snapshots)])
  (:use [simulator.problems.tracking.eventlog :only
	 (init-event-log add-entity add-event add-event-new add-event-move
			 update-entity get-entities get-events)])
  (:use [simulator.problems.tracking.grid :only
	 (new-grid new-entity update-grid-entity walk1 forward-time)])
  (:use [simulator.problems.tracking.sensors :only
	 (update-spotted generate-sensors-with-coverage measure-sensor-coverage
			 measure-sensor-overlap)])
  (:use [simulator.problems.tracking.hypotheses :only
	 (generate-hypotheses update-problem-data)])
  (:use [simulator.problems.tracking.positions :only
	 (manhattan-distance)])
  (:use [simulator.strategies :only (init-strat-state explain)]))

(defn add-new-entities
  [trueevents gridstate numes]
  (loop [i 0
	 te trueevents
	 gs gridstate]
    (if (or (= i numes) ; stop if reached numes or if there's no more space in grid
	    (= (* (:width (:grid gs)) (:height (:grid gs)))
	       (count (get-entities te))))
      [te gs]
      (let [entity (new-entity (:grid gs) (:time gridstate))]
	(recur (inc i)
	       (-> te
		   (add-entity entity)
		   (add-event-new 0 (pos entity)))
	       (update-grid-entity gs nil entity))))))

(defn init-states
  [width height numes]
  (let [trueevents (init-event-log)
	gridstate (GridState. (new-grid width height) 0)]
    (add-new-entities trueevents gridstate numes)))

(defn random-walks
  [walk trueevents gridstate]
  (let [time (:time gridstate) ;;; TODO: should time change for every move?
	entities (get-entities trueevents)
	entities-map (apply assoc {} (interleave entities entities))
	entity-walks (shuffle (apply concat (map #(repeat (rand-int (inc walk)) %)
						 entities)))]
    (loop [em entities-map
	   gs gridstate
	   ew entity-walks]
      (if (empty? ew)
	[(reduce (fn [te olde] (-> te
				 (update-entity time olde (pos (get em olde)))
				 (add-event-move time (pos olde) (pos (get em olde)))))
		 trueevents (keys em))
	 gs]
	(let [e (first ew)
	      olde (get em e)
	      newe (walk1 olde (:grid gs) time)]
	  (recur (assoc em e newe)
		 (update-grid-entity gs olde newe)
		 (rest ew)))))))

(defn possibly-add-new-entities
  [trueevents gridstate]
  (if (> 2.0 (rand)) [trueevents gridstate] ; skip adding new entities 95% of the time
      (add-new-entities trueevents gridstate (inc (rand-int 2)))))

(defn single-step
  [params sensors [trueevents gridstate strat-state]]
  (let [time (:time gridstate)
	sens (map #(update-spotted % gridstate) sensors)
	ss (generate-hypotheses strat-state sens time)
	ss2 (explain ss time)
	ss3 (update-problem-data ss2 time)
	[te gs] (random-walks (:MaxWalk params) trueevents (forward-time gridstate 1))
	[newte newgs] (possibly-add-new-entities te gs)]
    [newte newgs ss3]))

(defn last-explanation
  [sensors [trueevents gridstate strat-state]]
  (let [time (:time gridstate)
	sens (map #(update-spotted % gridstate) sensors)
	ss (generate-hypotheses strat-state sens time)
	ss2 (explain ss time)
	ss3 (update-problem-data ss2 time)]
    [trueevents ss3]))

(defn calc-average-walk
  [trueevents]
  (let [calc-walk-sum
	(fn [sum [es-old es-new]]
	  (+ sum (manhattan-distance (:pos es-old) (:pos es-new))))

	sum-walks
	(fn [pairs] (reduce calc-walk-sum 0 pairs))

	walk-avgs
	(map (fn [e] (let [pairs (pair-snapshots e)]
		       (if (empty? pairs) 0 (/ (sum-walks pairs) (count pairs)))))
	     (get-entities trueevents))]
    (double (/ (reduce + 0 walk-avgs) (count walk-avgs)))))

(defn evaluate
  [trueevents strat-state]
  (let [correct (count (intersection (set (get-events trueevents))
                                     (set (get-events (:problem-data strat-state)))))
	total (count (get-events trueevents))]
    (double (* 100 (/ correct total)))))

(defn run
  [params strat-state]
  (let [sensors (generate-sensors-with-coverage
		  (:GridWidth params) (:GridHeight params) (:SensorCoverage params))
	[trueevents gridstate] (init-states
				(:GridWidth params) (:GridHeight params)
				(:NumberEntities params))
	startTime (. System (nanoTime))]
    (loop [i 0
	   combined-states [trueevents gridstate strat-state]]
      (if (< i (:Steps params))
	(recur (inc i) (single-step params sensors combined-states))
	(let [[te ss] (last-explanation sensors combined-states)]
	  {:trueevents te :stratstate ss :sensors sensors :results
	   (assoc params
	     :Milliseconds (/ (double (- (. System (nanoTime)) startTime)) 1000000.0)
	     :PercentCorrect (evaluate te ss)
	     :Strategy (:strategy ss)
	     :StrategyCompute (:compute (:resources ss))
	     :StrategyMilliseconds (:milliseconds (:resources ss))
	     :StrategyMemory (:memory (:resources ss))
	     :AvgWalk (calc-average-walk te)
	     :SensorCoverage (measure-sensor-coverage
			      (:GridWidth params) (:GridHeight params) sensors)
	     :SensorOverlap (measure-sensor-overlap
			     (:GridWidth params) (:GridHeight params) sensors))})))))


