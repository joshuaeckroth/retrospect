(ns retrospect.problems.tracking.problem
  (:require [retrospect problem])
  (:import [retrospect.problem Problem])
  (:use [retrospect.problems.tracking.evaluate :only
         [evaluate evaluate-meta evaluate-comparative]])
  (:use [retrospect.problems.tracking.truedata :only
         [generate-truedata export-truedata]])
  (:use [retrospect.problems.tracking.sensors :only
         [generate-sensors]])
  (:use [retrospect.problems.tracking.hypotheses :only
         [hypothesize get-more-hyps commit-decision inconsistent]])
  (:use [retrospect.problems.tracking.player :only
         [player-get-params player-set-params player-get-params-panel
          player-get-stats-panel player-update-stats player-get-truedata-log
          player-get-problem-log player-setup-diagram player-update-diagram]])
  (:use [retrospect.problems.tracking.sensors :only
         [sensors-seen-grid]])
  (:use [retrospect.problems.tracking.monitor :only
         [monitor]])
  (:use [retrospect.problems.tracking.prepared :only
         [prepared-map]]))

(defn generate-problem-data
  [sensors params]
  {:paths (sorted-map)
   :split-merge-hyps []
   :log [] ;; log is reset each time by commit-decision
   :sensors-seen-grid (sensors-seen-grid sensors params)
   :spotted-grid []
   :uncovered #{}
   :sensors-seen []
   :sensors-unseen []
   :sensor-coverage 0.0 
   :sensor-overlap 0.0})

(def headers
     [:PEC :CountRemoved :CountRemovedPercent
      :PlausibilityWorkspaceAccuracy
      :MTL :MeanCountAlternatives :MLC :DistinctLabels :PlausibilityAccuracy
      :SensorOverlap :EntityDensity :NumberEntities :MaxWalk :ProbNewEntities
      :GridWidth :GridHeight :SensorCoverage :SensorSeesColor])

(def meta-headers
     [:AvgMetaDiffPEC 
      :AvgMetaDiffMTL 
      :AvgMetaDiffMLC])

(def comparative-headers
     [:MetaPEC :BasePEC :RatioPEC
      :IncreasePEC :MetaMTL :BaseMTL
      :RatioMTL :IncreaseMTL
      :MetaMLC :BaseMLC :RatioMLC
      :IncreaseMLC
      :MetaDistinctLabels :BaseDistinctLabels :RatioDistinctLabels
      :IncreaseDistinctLabels
      :MetaPlausibilityAccuracy :BasePlausibilityAccuracy :RatioPlausibilityAccuracy
      :IncreasePlausibilityAccuracy
      :MetaPlausibilityWorkspaceAccuracy :BasePlausibilityWorkspaceAccuracy
      :RatioPlausibilityWorkspaceAccuracy :IncreasePlausibilityWorkspaceAccuracy
      :NumberEntities :MaxWalk :ProbNewEntities :SensorSeesColor :GridWidth :GridHeight])

(def tracking-problem
     (Problem. "Tracking"
               headers
               meta-headers
               comparative-headers
               monitor
               {:get-params-fn player-get-params
                :set-params-fn player-set-params
                :get-params-panel-fn player-get-params-panel
                :get-stats-panel-fn player-get-stats-panel
                :update-stats-fn player-update-stats
                :get-truedata-log player-get-truedata-log
                :get-problem-log player-get-problem-log
                :setup-diagram-fn player-setup-diagram
                :update-diagram-fn player-update-diagram}
               generate-truedata
               generate-sensors
               export-truedata
               prepared-map
               hypothesize
               get-more-hyps
               commit-decision
               generate-problem-data
               inconsistent
               evaluate
               evaluate-meta
               evaluate-comparative))
