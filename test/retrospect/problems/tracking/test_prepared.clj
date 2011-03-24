(ns retrospect.problems.tracking.test-prepared
  (:use [midje.sweet])
  (:use [retrospect.workspaces :only [last-id get-conf]])
  (:use [retrospect.epistemicstates :only [previous-ep-state]])
  (:use [retrospect.onerun :only [init-one-run-state]])
  (:use [retrospect.problem :only [run-simulation run-simulation-step evaluate]])
  (:use [retrospect.problems.tracking.prepared])
  (:use [retrospect.problems.tracking.problem :only [tracking-problem]])
  (:use [retrospect.problems.tracking.hypotheses :only [paths-str]])
  (:use [retrospect.confidences]))

(defn run-for-or-state
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
      (loop [ors or-state]
        (if (>= (:time (:ep-state ors)) (:Steps params))
          (evaluate tracking-problem truedata ors params)
          (recur
           (run-simulation-step tracking-problem truedata ors params false true)))))))

(defn run-for-results
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

(let [results (run-for-results intersection-ambiguity)]
  (facts (:PercentEventsCorrect results) => (roughly 100.0)
         (:MetaAbductions results) => 1
         (:MeanTimeWithLabel results) => (roughly 3.0)))

(let [results (run-for-results intersection-ambiguity-nometa)]
  (facts (:PercentEventsCorrect results) => (roughly 0.0)
         (:MetaAbductions results) => 0
         (:MeanTimeWithLabel results) => (roughly 1.0)))

(let [or-state (run-for-or-state split-ambiguity)
      results (last (:results or-state))]
  (facts (:PercentEventsCorrect results) => (roughly 100.0)
         (:MeanTimeWithLabel results) => (roughly 2.0)
         (paths-str (:paths (:problem-data (:ep-state or-state))))
         => "A:[4,0@0 4,2@1 ]\nB:[4,2@1 2,4@2 2,6@3 ]\nC:[4,2@1 6,4@2 6,6@3 ]"))

(let [or-state (run-for-or-state split-ambiguity-2)
      results (last (:results or-state))]
  (facts (:PercentEventsCorrect results) => (roughly 100.0)
         (:MeanTimeWithLabel results) => (roughly 2.0)
         (paths-str (:paths (:problem-data (:ep-state or-state))))
         => "A:[1,0@0 1,1@1 ]\nB:[6,0@0 6,1@1 ]\nC:[1,1@1 0,2@2 0,3@3 ]\nD:[1,1@1 2,2@2 2,3@3 ]\nE:[6,1@1 5,2@2 5,3@3 ]\nF:[6,1@1 7,2@2 7,3@3 ]"))

(let [or-state (run-for-or-state merge-ambiguity)
      results (last (:results or-state))]
  (facts (:PercentEventsCorrect results) => (roughly 100.0)
         (:MeanTimeWithLabel results) => (roughly 1.66 0.1)
         (paths-str (:paths (:problem-data (:ep-state or-state))))
         => "A:[4,0@0 4,2@1 5,4@2 ]\nB:[6,0@0 6,2@1 5,4@2 ]\nC:[5,4@2 5,6@3 ]"))

(let [or-state (run-for-or-state merge-ambiguity-2)
      results (last (:results or-state))]
  (facts (:PercentEventsCorrect results) => (roughly 100.0)
         (:MeanTimeWithLabel results) => (roughly 1.66 0.1)
         (paths-str (:paths (:problem-data (:ep-state or-state))))
         => "A:[0,0@0 0,2@1 1,4@2 ]\nB:[2,0@0 2,2@1 1,4@2 ]\nC:[7,0@0 7,2@1 8,4@2 ]\nD:[9,0@0 9,2@1 8,4@2 ]\nE:[8,4@2 8,6@3 ]\nF:[1,4@2 1,6@3 ]"))

(let [or-state (run-for-or-state split-merge)
      results (last (:results or-state))]
  (facts (:PercentEventsCorrect results) => (roughly 100.0)
         (:MeanTimeWithLabel results) => (roughly 1.75)
         (:MeanLabelCounts results) => (roughly 3.50)
         (paths-str (:paths (:problem-data (:ep-state or-state))))
         => "A:[5,0@0 5,1@1 ]\nB:[5,1@1 3,3@2 3,4@3 5,5@4 ]\nC:[5,1@1 7,3@2 7,4@3 5,5@4 ]\nD:[5,5@4 5,6@5 ]"))

