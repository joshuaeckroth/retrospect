(ns samre.problems.tracking.problem
  (:require [samre problem])
  (:import [samre.problem Problem])
  (:use [samre.problems.tracking.evaluate :only (evaluate)])
  (:use [samre.problems.tracking.truedata :only (generate-truedata)])
  (:use [samre.problems.tracking.sensors :only (generate-sensors)])
  (:use [samre.problems.tracking.hypotheses :only
         (prepare-hyps get-more-hyps)])
  (:use [samre.problems.tracking.player :only
         [player-get-params player-get-params-panel
          player-get-diagram player-get-stats-panel
          player-update-stats player-update-truedata-log-box]])
  (:use [samre.problems.tracking.sensors :only
         (measure-sensor-overlap measure-sensor-coverage list-sensors-see)])
  (:use [samre.problems.tracking.eventlog :only (init-event-log)]))

(def avg-fields [:PercentEventsCorrect :PercentEventsWrong :PercentIdentitiesCorrect
		 :NumberEntities :MaxWalk :AvgWalk :PlausibilityAccuracy
		 :ProbNewEntities :GridWidth :GridHeight
                 :SensorCoverage :SensorOverlap])

(def non-avg-fields [])

(def charts
  [{:x :PlausibilityAccuracy :y :PercentEventsCorrect :name "pl-acc-events-correct"
    :each-reg :linear}
   {:x :NumberEntities :y :PlausibilityAccuracy :name "numes-pl-acc"
    :each-reg :linear}
   {:x :MetaAbductions :y :PlausibilityAccuracy :name "meta-abductions-pl-acc"
    :each-reg :linear}
   {:x :PercentEventsCorrect :y :MetaAbductions :name "events-correct-meta-abductions"
    :split-by :SensorCoverage :split-list (range 0 101 10) :split-delta 10
    :each-reg :linear}
   {:x :PercentEventsWrong :y :MetaAbductions :name "events-wrong-meta-abductions"
    :split-by :SensorCoverage :split-list (range 0 101 10) :split-delta 10
    :each-reg :linear}
   {:x :PercentIdentitiesCorrect :y :MetaAbductions :name "ids-wrong-meta-abductions"
    :split-by :SensorCoverage :split-list (range 0 101 10) :split-delta 10
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
    :each-reg :linear}
   {:x :SensorCoverage :y :PercentEventsWrong :name "coverage-events-wrong"
    :regression :linear
    :each-reg :linear}
   {:x :PercentEventsCorrect :y :PercentIdentitiesCorrect :name "events-correct-identities"
    :split-by :SensorCoverage :split-list (range 10 101 10) :split-delta 5
    :regression :linear}])

(defn generate-problem-data
  [params sensors]
  {:eventlog (init-event-log)
   :sensors-see (list-sensors-see (:GridWidth params) (:GridHeight params) sensors)
   :sensor-coverage (measure-sensor-coverage (:GridWidth params) (:GridHeight params)
                                             sensors)
   :sensor-overlap (measure-sensor-overlap (:GridWidth params) (:GridHeight params)
                                           sensors)})

(def tracking-problem
  (Problem. "tracking"
            prepare-hyps
            get-more-hyps
            {:get-params-fn player-get-params
             :get-params-panel-fn player-get-params-panel
             :get-diagram-fn player-get-diagram
             :get-stats-panel-fn player-get-stats-panel
             :update-stats-fn player-update-stats
             :update-truedata-log-box-fn player-update-truedata-log-box}
            generate-truedata generate-sensors
            evaluate generate-problem-data avg-fields non-avg-fields charts))
