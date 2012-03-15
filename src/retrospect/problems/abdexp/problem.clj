(ns retrospect.problems.abdexp.problem
  (:use [retrospect.problems.abdexp.truedata :only [generate-truedata]])
  (:use [retrospect.problems.abdexp.sensors :only [generate-sensors]])
  (:use [retrospect.reason.abdexp.player :only
         [player-get-stats-panel player-setup-diagram player-update-diagram]]))

(def abdexp-problem
  {:name "AbdExp"
   :player-fns {:get-stats-panel-fn player-get-stats-panel
                :update-stats-fn (constantly nil)
                :setup-diagram-fn player-setup-diagram
                :update-diagram-fn player-update-diagram
                :get-truedata-log (constantly "")
                :get-problem-log (constantly "")}
   :generate-truedata-fn generate-truedata
   :generate-sensors-fn generate-sensors
   :perturb-fn identity
   :prepared-map {}
   :default-params {:Steps [100 [100]]
                    :StepsBetween [1 [1]]
                    :NumVertices [40 [40]]
                    :MaxExplainLinks [10 [10]]
                    :MaxConflictLinks [10 [10]]
                    :PreferAbducibles [true [true false]]
                    :Scores [true [true false]]}})
