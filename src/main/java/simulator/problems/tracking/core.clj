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
  (:use [simulator.strategies :only (explain)]))

(defn add-new-entities
  [trueevents gridstate numes time]
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
		   (add-event-new time (pos entity)))
	       (update-grid-entity gs nil entity))))))

(defn init-states
  [width height numes]
  (let [trueevents (init-event-log)
	gridstate (GridState. (new-grid width height) 0)]
    (add-new-entities trueevents gridstate numes 0)))

(defn random-walks
  [trueevents gridstate params]
  (let [time (:time gridstate)
        all-entities (get-entities trueevents)
        entities (take (inc (int (* (count all-entities)
                                    (double (/ (:ProbMovement params) 100)))))
                       (shuffle all-entities))
	entities-map (apply assoc {} (interleave entities entities))
	entity-walks
        (shuffle (apply concat (map #(repeat (inc (rand-int (:MaxWalk params))) %)
						 entities)))]
    (loop [em entities-map
	   gs gridstate
	   ew entity-walks]
      (if (empty? ew)
        [(reduce (fn [te olde]
                   (if (= (pos olde) (pos (get em olde))) te
                     (-> te
                         (update-entity time olde (pos (get em olde)))
                         (add-event-move time (pos olde) (pos (get em olde))))))
                 trueevents (keys em))
         gs]
	(let [e (first ew)
	      olde (get em e)
	      newe (walk1 olde (:grid gs) time)]
          (if newe
            (recur (assoc em e newe)
                   (update-grid-entity gs olde newe)
                   (rest ew))
            (recur em gs (rest ew))))))))

(defn possibly-add-new-entities
  [trueevents gridstate params time]
  (if (>= (double (/ (:ProbNewEntities params) 100)) (rand))
    (add-new-entities trueevents gridstate 1 time)
    [trueevents gridstate]))

(defn single-step
  [params sensors [trueevents gridstate strat-states]]
  (let [time (:time gridstate)
        [te gs] (possibly-add-new-entities trueevents gridstate params time)
	sens (map #(update-spotted % gs) sensors)
	sss (map #(generate-hypotheses % sens time params) strat-states)
	sss2 (map #(explain % time) sss)
	sss3 (map #(update-problem-data % time) sss2)
	[newte newgs] (random-walks te (forward-time gs 1) params)]
    [newte newgs sss3]))

(defn last-explanation
  [params sensors [trueevents gridstate strat-states]]
  (let [time (:time gridstate)
	sens (map #(update-spotted % gridstate) sensors)
	sss (map #(generate-hypotheses % sens time params) strat-states)
	sss2 (map #(explain % time) sss)
	sss3 (map #(update-problem-data % time) sss2)]
    [trueevents sss3]))

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

(defn find-correct-identities
  [trueevents strat-state]
  "An 'identity' of an entity is its starting position (at a specific
   time). Here we compare starting positions with ending positions to
   see if the believed entities can also match start-end
   positions. This means a believed 'identity' is correct even when
   the inner path is wrong; only the beginning and end must match up
   with the true entities."
  (let [get-starts-ends (fn [es] (set (map (fn [e] {:start (first (:snapshots e))
                                                    :end (last (:snapshots e))})
                                           es)))
        true-starts-ends (get-starts-ends (get-entities trueevents))
        strat-starts-ends (get-starts-ends (get-entities (:problem-data strat-state)))]
    (intersection true-starts-ends strat-starts-ends)))

(defn evaluate
  [trueevents strat-state]
  (let [events-correct
        (count (intersection (set (get-events trueevents))
                             (set (get-events (:problem-data strat-state)))))
	events-total (count (get-events trueevents))
        identities-correct (count (find-correct-identities trueevents strat-state))
        identities-total (count (get-entities trueevents))]
    {:PercentEventsCorrect
     (double (* 100 (/ events-correct events-total)))
     :PercentIdentitiesCorrect
     (double (* 100 (/ identities-correct identities-total)))}))

(defn run
  [params strat-states]
  (let [sensors (generate-sensors-with-coverage
		  (:GridWidth params) (:GridHeight params) (:SensorCoverage params))
	[trueevents gridstate] (init-states
				(:GridWidth params) (:GridHeight params)
				(:NumberEntities params))
	startTime (. System (nanoTime))]
    (loop [i 0
	   combined-states [trueevents gridstate strat-states]]
      (if (< i (:Steps params))
	(recur (inc i) (single-step params sensors combined-states))
	(let [[te sss] (last-explanation params sensors combined-states)]
          (for [ss sss]
            {:trueevents te :stratstate ss :sensors sensors
             :results
             (merge (evaluate te ss)
                    (assoc params
                      :Milliseconds
                      (/ (double (- (. System (nanoTime)) startTime)) 1000000.0)
                      :Strategy (:strategy ss)
                      :StrategyCompute (:compute (:resources ss))
                      :StrategyMilliseconds (:milliseconds (:resources ss))
                      :StrategyMemory (:memory (:resources ss))
                      :AvgWalk (calc-average-walk te)
                      :SensorCoverage
                      (measure-sensor-coverage
                       (:GridWidth params) (:GridHeight params) sensors)
                      :SensorOverlap
                      (measure-sensor-overlap
                       (:GridWidth params) (:GridHeight params) sensors)))}))))))


