(ns samre.problems.tracking.problem
  (:require [samre problem])
  (:import [samre.problem Problem])
  (:use [samre.problems.tracking.evaluate :only [evaluate]])
  (:use [samre.problems.tracking.truedata :only [generate-truedata]])
  (:use [samre.problems.tracking.sensors :only [ generate-sensors]])
  (:use [samre.problems.tracking.hypotheses :only
         [hypothesize commit-decision]])
  (:use [samre.problems.tracking.player :only
         [player-get-params player-set-params player-get-params-panel
          player-get-diagram player-get-stats-panel
          player-update-stats player-update-truedata-log-box
          player-update-problem-log-box]])
  (:use [samre.problems.tracking.sensors :only
         [measure-sensor-overlap measure-sensor-coverage
          list-sensors-seen list-sensors-unseen sensors-seen-grid]])
  (:use [samre.problems.tracking.monitor :only [monitor]])
  (:use [samre.problems.tracking.prepared :only [prepared-map]]))

(def avg-fields [:PercentEventsCorrect :MeanTimeWithLabel :MaxTimeWithLabel
                 :MinTimeWithLabel :MeanCountAlternatives :MeanLabelCounts
		 :NumberEntities :MaxWalk :AvgWalk :PlausibilityAccuracy
		 :ProbNewEntities :GridWidth :GridHeight
                 :SensorCoverage :SensorSeesColor :SensorOverlap :EntityDensity])

(def non-avg-fields [])

(def charts
  [{:x :ProbNewEntities :y :PercentEventsCorrect :name "probnew-events-correct"
    :each-reg :linear}
   {:x :SensorCoverage :y :Unexplained :name "coverage-unexplained"
    :each-reg :linear}
   {:x :NumberEntities :y :Unexplained :name "numes-unexplained"
    :each-reg :linear}
   {:x :EntityDensity :y :PercentEventsCorrect :name "density-events-correct"
    :each-reg :linear}
   {:x :StepsBetween :y :PercentEventsCorrect :name "stepsbetween-events-correct"
    :each-reg :linear}
   {:x :StepsBetween :y :PercentEventsCorrect :name "stepsbetween-events-correct"
    :each-reg :linear
    :split-by :SensorCoverage :split-list (range 0 101 10) :split-delta 10}
   {:x :PlausibilityAccuracy :y :PercentEventsCorrect :name "pl-acc-events-correct"
    :each-reg :linear}
   {:x :NumberEntities :y :PlausibilityAccuracy :name "numes-pl-acc"
    :each-reg :linear}
   {:x :PercentEventsCorrect :y :MeanTimeWithLabel :name "mean-time-label-events-correct"
    :split-by :SensorSeesColor :split-list (range 0 101 10) :split-delta 10
    :each-reg :linear}
   {:x :MetaAbductions :y :PercentEventsCorrect :name "meta-abductions-events-correct"
    :split-by :SensorSeesColor :split-list (range 0 101 10) :split-delta 10
    :each-reg :linear}
   {:x :MetaAbductions :y :MeanTimeWithLabel :name "meta-abductions-mean-time-label"
    :split-by :SensorSeesColor :split-list (range 0 101 10) :split-delta 10
    :each-reg :linear}
   {:x :MetaAbductions :y :MeanCountAlternatives :name "meta-abductions-mean-count-alts"
    :split-by :SensorSeesColor :split-list (range 0 101 10) :split-delta 10
    :each-reg :linear}
   {:x :PercentEventsCorrect :y :MetaAbductions :name "events-correct-meta-abductions"
    :split-by :SensorSeesColor :split-list (range 0 101 10) :split-delta 10
    :each-reg :linear}
   {:x :MetaAbductions :y :Milliseconds :name "meta-abductions-milliseconds"
    :each-reg :linear}
   {:x :PercentEventsCorrect :y :PercentEventsWrong :name "events-correct-wrong"
    :split-by :SensorCoverage :split-list (range 0 101 10) :split-delta 10
    :each-reg :linear}
   {:x :NumberEntities :y :PercentEventsCorrect :name "numes-events-correct"
    :split-by :SensorCoverage :split-list (range 0 101 10) :split-delta 10
    :y-range [0.0 100.0]
    :each-reg :linear}
   {:x :NumberEntities :y :PercentEventsWrong :name "numes-events-wrong"
    :split-by :SensorCoverage :split-list (range 0 101 10) :split-delta 10
    :each-reg :linear}
   {:x :AvgWalk :y :PercentEventsCorrect :name "avgwalk-events"
    :split-by :SensorCoverage :split-list (range 0 101 10) :split-delta 10
    :y-range [0.0 100.0]
    :each-reg :linear}
   {:x :NumberEntities :y :PercentIdentitiesCorrect :name "numes-ids"
    :split-by :SensorCoverage :split-list (range 0 101 10) :split-delta 10
    :y-range [0.0 100.0]
    :each-reg :linear}
   {:x :AvgWalk :y :PercentIdentitiesCorrect :name "avgwalk-ids"
    :split-by :SensorCoverage :split-list (range 0 101 10) :split-delta 10
    :y-range [0.0 100.0]
    :each-reg :linear}
   {:x :AvgWalk :y :Milliseconds :name "avgwalk-milliseconds"
    :split-by :SensorCoverage :split-list (range 0 101 10) :split-delta 10
    :regression :linear
    :each-reg :linear}
   {:x :SensorCoverage :y :PercentEventsCorrect :name "coverage-events-correct"
    :regression :linear
    :each-reg :linear}])

(defn generate-problem-data
  [sensors params]
  {:paths {}
   :sensors-seen-grid (sensors-seen-grid sensors params)
   :spotted-grid []
   :sensors-seen
   (list-sensors-seen (:GridWidth params) (:GridHeight params) sensors)
   :sensors-unseen
   (list-sensors-unseen (:GridWidth params) (:GridHeight params) sensors)
   :sensor-coverage
   (measure-sensor-coverage (:GridWidth params) (:GridHeight params) sensors)
   :sensor-overlap
   (measure-sensor-overlap (:GridWidth params) (:GridHeight params) sensors)})

(def tracking-problem
  (Problem. "tracking"
            monitor
            {:get-params-fn player-get-params
             :set-params-fn player-set-params
             :get-params-panel-fn player-get-params-panel
             :get-diagram-fn player-get-diagram
             :get-stats-panel-fn player-get-stats-panel
             :update-stats-fn player-update-stats
             :update-truedata-log-box-fn player-update-truedata-log-box
             :update-problem-log-box-fn player-update-problem-log-box}
            generate-truedata
            generate-sensors
            prepared-map
            hypothesize
            commit-decision
            generate-problem-data
            evaluate
            avg-fields non-avg-fields charts))
