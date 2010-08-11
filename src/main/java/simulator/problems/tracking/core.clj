(ns simulator.problems.tracking.core
  (:require [simulator.types states problem])
  (:require [simulator.problems.tracking grid])
  (:import [simulator.types.states State])
  (:import [simulator.types.problem Problem])
  (:import [simulator.problems.tracking.grid GridState])
  (:use [simulator.evaluator :only (evaluate)])
  (:use [simulator.types.generic :only (forward-time)])
  (:use [simulator.types.entities :only (pos)])
  (:use [simulator.types.states :only
	 (add-entity add-event-new add-event-move update-entity get-entities)])
  (:use [simulator.problems.tracking.grid :only
	 (new-grid new-entity update-grid-entity walk1)])
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
  [truestate gridstate numes]
  (loop [i 0
	 ts truestate
	 gs gridstate]
    (if (or (= i numes) ; stop if reached numes or if there's no more space in grid
	    (= (* (:width (:grid gs)) (:height (:grid gs)))
	       (count (get-entities ts))))
      [ts gs]
      (let [entity (new-entity (:grid gs))]
	(recur (inc i)
	       (-> ts
		   (add-entity entity)
		   (add-event-new 0 (pos entity)))
	       (update-grid-entity gs nil entity))))))

(defn init-states
  [width height numes]
  (let [truestate (State. [] [])
	gridstate (GridState. (new-grid width height) 0)]
    (add-new-entities truestate gridstate numes)))

(defn random-walks
  [walk truestate gridstate]
  (let [time (:time gridstate) ;;; TODO: should time change for every move?
	entities (get-entities truestate)
	entities-map (apply assoc {} (interleave entities entities))
	entity-walks (shuffle (apply concat (map #(repeat (rand-int (inc walk)) %) entities)))]
    (loop [em entities-map
	   gs gridstate
	   ew entity-walks]
      (if (empty? ew)
	[(reduce (fn [ts olde] (-> ts
				 (update-entity olde (pos (get em olde)))
				 (add-event-move time (pos olde) (pos (get em olde)))))
		 truestate (keys em))
	 gs]
	(let [e (first ew)
	      olde (get em e)
	      newe (walk1 olde (:grid gs))]
	  (recur (assoc em e newe)
		 (update-grid-entity gs olde newe)
		 (rest ew)))))))

(defn possibly-add-new-entities
  [truestate gridstate]
  (if (> 2.0 (rand)) [truestate gridstate] ; skip adding new entities 95% of the time
      (add-new-entities truestate gridstate (inc (rand-int 2)))))

(defn single-step
  [params sensors [truestate gridstate strat-state]]
  (let [sens (map #(update-spotted % gridstate) sensors)
	strat-s (explain strat-state sens (:time gridstate))
	[ts gs] (random-walks (:MaxWalk params) truestate (forward-time gridstate 1))
	[newts newgs] (possibly-add-new-entities ts gs)]
    [newts newgs strat-s]))

(defn last-explanation
  [sensors [truestate gridstate strat-state]]
  [truestate (explain strat-state (map #(update-spotted % gridstate) sensors)
		      (:time gridstate))])

(defn run
  [params strat-state]
  (let [sensors (generate-sensors-with-coverage
		  (:GridWidth params) (:GridHeight params) (:SensorCoverage params))
	[truestate gridstate] (init-states
			       (:GridWidth params) (:GridHeight params) (:NumberEntities params))
	startTime (. System (nanoTime))]
    (loop [i 0
	   combined-states [truestate gridstate strat-state]]
      (if (< i (:Steps params))
	(recur (inc i) (single-step params sensors combined-states))
	(let [[ts ss] (last-explanation sensors combined-states)]
	  {:truestate ts :stratstate ss :sensors sensors :results
	   (assoc params
	     :Milliseconds (/ (double (- (. System (nanoTime)) startTime)) 1000000.0)
	     :PercentCorrect (evaluate ts ss)
	     :Strategy (:strategy ss)
	     :StrategyCompute 0
	     :StrategyMilliseconds 0
	     :StrategyMemory 0
	     :AvgWalk 0
	     :SensorCoverage (measure-sensor-coverage (:GridWidth params) (:GridHeight params) sensors)
	     :SensorOverlap 0)})))))

(def tracking-problem
     (Problem. "tracking" run headers avg-fields non-avg-fields))

