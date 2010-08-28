(ns simulator.problems.tracking.core
  (:require [simulator.types problem])
  (:require [simulator.problems.tracking eventlog grid])
  (:import [simulator.problems.tracking.eventlog EventLog])
  (:import [simulator.types.problem Problem])
  (:import [simulator.problems.tracking.grid GridState])
  (:use [simulator.evaluator :only (evaluate)])
  (:use [simulator.problems.tracking.entities :only (pos)])
  (:use [simulator.problems.tracking.eventlog :only
	 (add-entity add-event-new add-event-move update-entity get-entities)])
  (:use [simulator.problems.tracking.grid :only
	 (new-grid new-entity update-grid-entity walk1 forward-time)])
  (:use [simulator.problems.tracking.sensors :only
	 (update-spotted generate-sensors-with-coverage measure-sensor-coverage)])
  (:use [simulator.strategies.core :only (init-strat-state)])
  (:use [simulator.strategies.explain :only (explain)]))

(def avg-fields [:Milliseconds :PercentCorrect
		 :StrategyCompute :StrategyMilliseconds :StrategyMemory
		 :Steps :NumberEntities :MaxWalk :AvgWalk
		 :ProbNewEntities :SensorReportNoise :BeliefNoise
		 :GridWidth :GridHeight :SensorCoverage :SensorOverlap])

(def non-avg-fields [:Strategy])

(def headers (concat avg-fields non-avg-fields))

(defn add-new-entities
  [trueevents gridstate numes]
  (loop [i 0
	 te trueevents
	 gs gridstate]
    (if (or (= i numes) ; stop if reached numes or if there's no more space in grid
	    (= (* (:width (:grid gs)) (:height (:grid gs)))
	       (count (get-entities te))))
      [te gs]
      (let [entity (new-entity (:grid gs))]
	(recur (inc i)
	       (-> te
		   (add-entity entity)
		   (add-event-new 0 (pos entity)))
	       (update-grid-entity gs nil entity))))))

(defn init-states
  [width height numes]
  (let [trueevents (EventLog. [] [])
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
				 (update-entity olde (pos (get em olde)))
				 (add-event-move time (pos olde) (pos (get em olde)))))
		 trueevents (keys em))
	 gs]
	(let [e (first ew)
	      olde (get em e)
	      newe (walk1 olde (:grid gs))]
	  (recur (assoc em e newe)
		 (update-grid-entity gs olde newe)
		 (rest ew)))))))

(defn possibly-add-new-entities
  [trueevents gridstate]
  (if (> 2.0 (rand)) [trueevents gridstate] ; skip adding new entities 95% of the time
      (add-new-entities trueevents gridstate (inc (rand-int 2)))))

(defn single-step
  [params sensors [trueevents gridstate strat-state]]
  (let [sens (map #(update-spotted % gridstate) sensors)
	strat-s (explain strat-state sens (:time gridstate))
	[te gs] (random-walks (:MaxWalk params) trueevents (forward-time gridstate 1))
	[newte newgs] (possibly-add-new-entities ts gs)]
    [newte newgs strat-s]))

(defn last-explanation
  [sensors [trueevents gridstate strat-state]]
  [trueevents (explain strat-state (map #(update-spotted % gridstate) sensors)
		       (:time gridstate))])

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
	     :StrategyCompute 0
	     :StrategyMilliseconds 0
	     :StrategyMemory 0
	     :AvgWalk 0
	     :SensorCoverage (measure-sensor-coverage
			      (:GridWidth params) (:GridHeight params) sensors)
	     :SensorOverlap 0)})))))

(def tracking-problem
     (Problem. "tracking" run headers avg-fields non-avg-fields (EventLog. [] [])))

