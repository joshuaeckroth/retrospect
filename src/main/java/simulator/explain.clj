(ns simulator.explain
  (:use [simulator.types.entities :only (pos)])
  (:use [simulator.types.states :only (addEntity addEventNew addEventMove updateEntity)]))

(defn explain-new-entity
  [strat-state spotted time]
  (-> strat-state
      (addEntity spotted)
      (addEventNew time (pos spotted))))

(defn explain-existing-entity
  [strat-state spotted entity time]
  (-> strat-state
      (updateEntity entity (pos spotted))
      (addEventMove time (pos entity) (pos spotted))))

