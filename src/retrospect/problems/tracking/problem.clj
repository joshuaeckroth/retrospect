(ns retrospect.problems.tracking.problem
  (:require [retrospect problem])
  (:import [retrospect.problem Problem])
  (:use [retrospect.problems.tracking.evaluate :only [evaluate evaluate-comparative]])
  (:use [retrospect.problems.tracking.truedata :only [generate-truedata]])
  (:use [retrospect.problems.tracking.sensors :only [generate-sensors]])
  (:use [retrospect.problems.tracking.hypotheses :only
         [hypothesize commit-decision consistent?]])
  (:use [retrospect.problems.tracking.player :only
         [player-get-params player-set-params player-get-params-panel
          player-get-stats-panel player-update-stats player-get-truedata-log
          player-get-problem-log player-setup-diagram player-update-diagram]])
  (:use [retrospect.problems.tracking.sensors :only
         [measure-sensor-overlap measure-sensor-coverage
          list-sensors-seen list-sensors-unseen sensors-seen-grid]])
  (:use [retrospect.problems.tracking.monitor :only [monitor]])
  (:use [retrospect.problems.tracking.prepared :only [prepared-map]]))

(defn generate-problem-data
  [sensors params]
  {:paths (sorted-map)
   :sensors-seen-grid (sensors-seen-grid sensors params)
   :spotted-grid []
   :uncovered #{}
   :sensors-seen
   (list-sensors-seen (:GridWidth params) (:GridHeight params) sensors)
   :sensors-unseen
   (list-sensors-unseen (:GridWidth params) (:GridHeight params) sensors)
   :sensor-coverage
   (measure-sensor-coverage (:GridWidth params) (:GridHeight params) sensors)
   :sensor-overlap
   (measure-sensor-overlap (:GridWidth params) (:GridHeight params) sensors)})

(def headers
     [:PercentEventsCorrect :PlausibilityWorkspaceAccuracy
      :MeanTimeWithLabel :MaxTimeWithLabel :MinTimeWithLabel
      :MeanCountAlternatives :MeanLabelCounts :DistinctLabels :PlausibilityAccuracy
      :SensorOverlap :EntityDensity :NumberEntities :MaxWalk :ProbNewEntities
      :GridWidth :GridHeight :SensorCoverage :SensorSeesColor
      :PlausibilityVIEvents :PlausibilityIEvents :PlausibilityNEvents
      :PlausibilityPEvents :PlausibilityVPEvents])

(def comparative-headers
  [:MetaPercentEventsCorrect :BasePercentEventsCorrect :RatioPercentEventsCorrect
   :IncreasePercentEventsCorrect :MetaMeanTimeWithLabel :BaseMeanTimeWithLabel
   :RatioMeanTimeWithLabel :IncreaseMeanTimeWithLabel
   :MetaMaxTimeWithLabel :BaseMaxTimeWithLabel :RatioMaxTimeWithLabel
   :IncreaseMaxTimeWithLabel
   :MetaMinTimeWithLabel :BaseMinTimeWithLabel :RatioMinTimeWithLabel
   :IncreaseMinTimeWithLabel
   :MetaMeanLabelCounts :BaseMeanLabelCounts :RatioMeanLabelCounts
   :IncreaseMeanLabelCounts
   :MetaDistinctLabels :BaseDistinctLabels :RatioDistinctLabels
   :IncreaseDistinctLabels
   :MetaPlausibilityAccuracy :BasePlausibilityAccuracy :RatioPlausibilityAccuracy
   :IncreasePlausibilityAccuracy
   :NumberEntities :MaxWalk :ProbNewEntities])

(def tracking-problem
  (Problem. "Tracking"
            headers
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
            prepared-map
            hypothesize
            commit-decision
            generate-problem-data
            consistent?
            evaluate
            evaluate-comparative))
