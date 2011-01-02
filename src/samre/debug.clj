(ns samre.debug)

(def p
  (assoc (first (samre.records/explode-params
                 (samre.records/read-params
                  samre.problems.tracking.problem/tracking-problem
                  "params.xml")))
    :Strategies ["guess"] :MetaStrategies ["guess"]))

(samre.problem/run-strategies
 samre.problems.tracking.problem/tracking-problem p)

