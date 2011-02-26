(ns samre.problem
  (:import (java.util.concurrent ExecutionException))
  (:use [samre.onerun :only
         [init-one-run-states update-one-run-state proceed-one-run-state]])
  (:use [samre.epistemicstates :only
         [explain previous-ep-state current-ep-state]])
  (:use [samre.meta.explain :only [explain-meta]])
  (:use [samre.sensors :only [update-sensors]]))

(def avg-fields
  [:Steps :Step :StepsBetween :MetaAbductions :Milliseconds
   :SensorReportNoise :BeliefNoise :Unexplained :ExplainCycles
   :HypothesisCount :HypothesesNew])

(def non-avg-fields
  [:MetaAbduction :Lazy])

(defn evaluate
  [problem truedata or-state params]
  (let [prev-ep (previous-ep-state (:ep-state-tree or-state))]
    (update-in or-state [:results] conj
               (merge ((:evaluate-fn problem)
                       (:ep-state or-state)
                       (:sensors or-state) truedata params)
                      (assoc params
                        :Step (:time (:ep-state or-state))
                        :MetaAbduction (:meta-abduction or-state)
                        :Lazy (:lazy or-state)
                        :MetaAbductions (:meta-abductions (:resources or-state))
                        :Milliseconds (:milliseconds (:resources or-state))
                        :Unexplained
                        (if prev-ep (count (:unexplained (:workspace prev-ep))) 0)
                        :ExplainCycles
                        (:explain-cycles (:resources (:workspace prev-ep)))
                        :HypothesisCount
                        (:hyp-count (:resources (:workspace prev-ep)))
                        :HypothesesNew
                        (:hyps-new (:resources (:workspace prev-ep))))))))

(defn proceed-n-steps
  [n time truedata or-state]
  (loop [t time
         ors or-state]
    (if (= t (+ n time)) ors
        (recur (inc t) (update-in ors [:sensors] update-sensors (get truedata t) t)))))

(defn hypothesize
  [problem or-state time-now params]
  (update-one-run-state or-state
   ((:hypothesize-fn problem) (:ep-state or-state) (:sensors or-state) time-now params)))

(defn run-simulation-step
  [problem truedata or-state params monitor player]
  (let [time (:time (:ep-state or-state))
        ors-sensors (proceed-n-steps (:StepsBetween params) time truedata or-state)
        time-now (+ (dec (:StepsBetween params)) time)
        start-time (. System (nanoTime)) ;; start the clock
        ors-hyps (hypothesize problem ors-sensors time-now params)
        ep-explained (explain (:ep-state ors-hyps) params)
        ors-expl (update-one-run-state ors-sensors ep-explained)
        ors-meta (explain-meta problem ors-expl params)
        ep-meta (current-ep-state (:ep-state-tree ors-meta))
        ors-next (proceed-one-run-state ors-meta ep-meta time-now problem)
        ms (/ (- (. System (nanoTime)) start-time) 1000000.0) ;; stop the clock
        ors-resources (update-in ors-next [:resources] assoc :milliseconds ms)
        ors-results (evaluate problem truedata ors-resources params)]
    (if (and (not player) monitor)
      ((:monitor-fn problem) problem truedata (:sensors ors-results) ors-results params)
      ors-results)))

(defn run-simulation
  [problem truedata or-state monitor params]
  (loop [ors or-state]
    (when (nil? ors) (throw (ExecutionException. "Monitor took control." (Throwable.))))
    (if (>= (:time (:ep-state ors)) (:Steps params)) (:results ors)
        (recur (run-simulation-step problem truedata ors params monitor false)))))

(defn run-comparative
  [problem monitor params]
  (let [truedata ((:truedata-fn problem) params)
        sensors ((:sensor-gen-fn problem) params)
        problem-data ((:gen-problem-data-fn problem) sensors params)
        or-states (init-one-run-states {:MetaAbduction [true false] :Lazy [true]}
                                       sensors problem-data)]
    (doall (for [ors or-states]
             (last (run-simulation problem truedata ors monitor params))))))

(defn calc-percent-improvement
  [k m b]
  (if (= 0 (k b)) 0.0
      (double (* 100.0 (/ (- (k m) (k b)) (k b))))))

(defn calc-ratio
  [k m b]
  (if (= 0 (k b)) 0.0
      (double (/ (k m) (k b)))))

(defn run-many
  [problem monitor params n]
  (for [i (range n)]
    (let [[m b] (run-comparative problem monitor params)]
      {:MetaAbductions (:MetaAbductions m)
       :MetaMilliseconds (:Milliseconds m)
       :BaseMilliseconds (:Milliseconds b)
       :RatioMilliseconds (calc-ratio :Milliseconds m b)
       :ImproveMilliseconds (calc-percent-improvement :Milliseconds m b)
       :MetaUnexplained (:Unexplained m)
       :BaseUnexplained (:Unexplained b)
       :RatioUnexplained (calc-ratio :Unexplained m b)
       :MetaPercentEventsCorrect (:PercentEventsCorrect m)
       :BasePercentEventsCorrect (:PercentEventsCorrect b)
       :RatioPercentEventsCorrect (calc-ratio :PercentEventsCorrect m b)
       :ImprovePercentEventsCorrect (calc-percent-improvement :PercentEventsCorrect m b)
       :MetaMeanTimeWithLabel (:MeanTimeWithLabel m)
       :BaseMeanTimeWithLabel (:MeanTimeWithLabel b)
       :RatioMeanTimeWithLabel (calc-ratio :MeanTimeWithLabel m b)
       :ImproveMeanTimeWithLabel (calc-percent-improvement :MeanTimeWithLabel m b)
       :MetaExplainCycles (:ExplainCycles m)
       :BaseExplainCycles (:ExplainCycles b)
       :RatioExplainCycles (calc-ratio :ExplainCycles m b)
       :ImproveExplainCycles (calc-percent-improvement :ExplainCycles m b)
       :NumberEntities (:NumberEntities params)
       :MaxWalk (:MaxWalk params)
       :Steps (:Steps params)
       :ProbNewEntities (:ProbNewEntities params)})))

(defn average-runs
  [problem monitor params n]
  (doall (run-many problem monitor params n)))

(defn get-headers
  [problem]
  [:MetaAbductions
   :MetaMilliseconds
   :BaseMilliseconds
   :RatioMilliseconds
   :ImproveMilliseconds
   :MetaUnexplained
   :BaseUnexplained
   :RatioUnexplained
   :MetaPercentEventsCorrect
   :BasePercentEventsCorrect
   :RatioPercentEventsCorrect
   :ImprovePercentEventsCorrect
   :MetaMeanTimeWithLabel
   :BaseMeanTimeWithLabel
   :RatioMeanTimeWithLabel
   :ImproveMeanTimeWithLabel
   :MetaExplainCycles
   :BaseExplainCycles
   :RatioExplainCycles
   :ImproveExplainCycles
   :NumberEntities
   :MaxWalk
   :Steps
   :ProbNewEntities])

(defrecord Problem
    [name monitor-fn player-fns truedata-fn sensor-gen-fn prepared-map
     hypothesize-fn commit-decision-fn gen-problem-data-fn
     evaluate-fn avg-fields non-avg-fields charts])
