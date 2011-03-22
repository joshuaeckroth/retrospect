(ns retrospect.problems.tracking.test-prepared
  (:use midje.sweet)
  (:use [retrospect.workspaces :only [last-id]])
  (:use [retrospect.onerun :only [init-one-run-state]])
  (:use [retrospect.problem :only [run-simulation]])
  (:use retrospect.problems.tracking.prepared)
  (:use [retrospect.problems.tracking.problem :only [tracking-problem]]))

(defn run-prepared
  [prepared]
  (let [params (:params prepared)
        meta-abduction (:MetaAbduction params)
        lazy (:Lazy params)
        sensors (:sensors prepared)
        truedata (:truedata prepared)
        or-state (init-one-run-state meta-abduction lazy sensors
                                     ((:gen-problem-data-fn tracking-problem)
                                      sensors params))]
    (binding [last-id 0]
      (run-simulation tracking-problem truedata or-state params false))))

(let [results (run-prepared intersection-ambiguity)]
  (facts (:PercentEventsCorrect results) => (roughly 100.0)
         (:MetaAbductions results) => 1))

(let [results (run-prepared intersection-ambiguity-nometa)]
  (facts (:PercentEventsCorrect results) => (roughly 0.0)
         (:MetaAbductions results) => 0))

