(ns simulator.debug)

(def p
  (assoc (first (simulator.records/explode-params
                 (simulator.records/read-params
                  simulator.problems.tracking.problem/tracking-problem
                  "params.xml")))
    :Strategies ["guess"] :MetaStrategies ["guess"]))

(simulator.problem/run-strategies
 simulator.problems.tracking.problem/tracking-problem p)

