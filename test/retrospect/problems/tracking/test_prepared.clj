(ns retrospect.problems.tracking.test-prepared
  (:use [midje.sweet])
  (:use [retrospect.workspaces :only [last-id get-conf]])
  (:use [retrospect.epistemicstates :only [previous-ep-state]])
  (:use [retrospect.onerun :only [init-one-run-state]])
  (:use [retrospect.problem :only [run-simulation run-simulation-step evaluate]])
  (:use [retrospect.problems.tracking.prepared])
  (:use [retrospect.problems.tracking.problem :only [tracking-problem]])
  (:use [retrospect.problems.tracking.hypotheses :only [paths-str]])
  (:use [retrospect.confidences])
  (:use [retrospect.random]))

(defn run-for-or-state
  [prepared]
  (set-seed 10)
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
  (set-seed 10)
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

;; color-update
(let [or-state (run-for-or-state color-update)
      results (last (:results or-state))]
  (facts (:PercentEventsCorrect results) => (roughly 100.0)
         (:MeanTimeWithLabel results) => (roughly 3.0)
         (:MeanLabelCounts results) => (roughly 1.0)
         (paths-str (:paths (:problem-data (:ep-state or-state))))
         => "A (red): 9,5@0 -> 5,5@1 -> 2,5@2"))

;; color-update-2
(let [or-state (run-for-or-state color-update-2)
      results (last (:results or-state))]
  (facts (:PercentEventsCorrect results) => (roughly 100.0)
         (:MeanTimeWithLabel results) => (roughly 3.0)
         (:MeanLabelCounts results) => (roughly 1.0)
         (paths-str (:paths (:problem-data (:ep-state or-state))))
         => "A (red): 9,5@0 -> 5,5@1 -> 2,5@2"))

;; intersections
(let [results (run-for-results intersection-ambiguity)]
  (facts (:PercentEventsCorrect results) => (roughly 100.0)
         (:MetaAbductions results) => 1
         (:MeanTimeWithLabel results) => (roughly 3.0)
         (:MeanLabelCounts results) => (roughly 1.0)))

(let [or-state (run-for-or-state intersection-ambiguity-nometa)
      results (last (:results or-state))]
  (facts (:PercentEventsCorrect results) => (roughly 50.0)
         (:MetaAbductions results) => 0
         (:MeanTimeWithLabel results) => (roughly 1.0)
         (:MeanLabelCounts results) => (roughly 3.0)
         (paths-str (:paths (:problem-data (:ep-state or-state))))
         => "A (red): 5,7@0 -> 4,7@1\nB (blue): 5,4@0 -> 4,4@1\nC (red): 4,4@1 -> 2,3@2\nD (blue): 4,7@1 -> 2,8@2"))

(let [results (run-for-results intersection-ambiguity-nometa-allatonce)]
  (facts (:PercentEventsCorrect results) => (roughly 100.0)
         (:MetaAbductions results) => 0
         (:MeanTimeWithLabel results) => (roughly 3.0)
         (:MeanLabelCounts results) => (roughly 1.0)))

;; splits
(let [or-state (run-for-or-state split-ambiguity)
      results (last (:results or-state))]
  (facts (:PercentEventsCorrect results) => (roughly 100.0)
         (:MeanTimeWithLabel results) => (roughly 2.0)
         (:MeanLabelCounts results) => (roughly 2.0)
         (paths-str (:paths (:problem-data (:ep-state or-state))))
         => "A (red): 4,0@0 -> 4,2@1\nB (red): 4,2@1 -> 2,4@2 -> 2,6@3\nC (red): 4,2@1 -> 6,4@2 -> 6,6@3"))

(let [or-state (run-for-or-state split-ambiguity-2)
      results (last (:results or-state))]
  (facts (:PercentEventsCorrect results) => (roughly 100.0)
         (:MeanTimeWithLabel results) => (roughly 2.0)
         (:MeanLabelCounts results) => (roughly 2.0)
         (paths-str (:paths (:problem-data (:ep-state or-state))))
         => "A (red): 1,0@0 -> 1,1@1\nB (blue): 6,0@0 -> 6,1@1\nC (red): 1,1@1 -> 0,2@2 -> 0,3@3\nD (red): 1,1@1 -> 2,2@2 -> 2,3@3\nE (blue): 6,1@1 -> 7,2@2 -> 7,3@3\nF (blue): 6,1@1 -> 5,2@2 -> 5,3@3"))


;; merges
(let [or-state (run-for-or-state merge-ambiguity)
      results (last (:results or-state))]
  (facts (:PercentEventsCorrect results) => (roughly 100.0)
         (:MeanTimeWithLabel results) => (roughly 1.66 0.1)
         (:MeanLabelCounts results) => (roughly 2.5)
         (paths-str (:paths (:problem-data (:ep-state or-state))))
         => "A (red): 4,0@0 -> 4,2@1 -> 5,4@2\nB (red): 6,0@0 -> 6,2@1 -> 5,4@2\nC (red): 5,4@2 -> 5,6@3"))

(let [or-state (run-for-or-state merge-ambiguity-2)
      results (last (:results or-state))]
  (facts (:PercentEventsCorrect results) => (roughly 100.0)
         (:MeanTimeWithLabel results) => (roughly 1.66 0.1)
         (:MeanLabelCounts results) => (roughly 2.5)
         (paths-str (:paths (:problem-data (:ep-state or-state))))
         => "A (blue): 7,0@0 -> 7,2@1 -> 8,4@2\nB (red): 2,0@0 -> 2,2@1 -> 1,4@2\nC (blue): 9,0@0 -> 9,2@1 -> 8,4@2\nD (red): 0,0@0 -> 0,2@1 -> 1,4@2\nE (blue): 8,4@2 -> 8,6@3\nF (red): 1,4@2 -> 1,6@3"))


;; splits + merges
(let [or-state (run-for-or-state split-merge)
      results (last (:results or-state))]
  (facts (:PercentEventsCorrect results) => (roughly 100.0)
         (:MeanTimeWithLabel results) => (roughly 1.75)
         (:MeanLabelCounts results) => (roughly 3.50)
         (paths-str (:paths (:problem-data (:ep-state or-state))))
         => "A (red): 5,0@0 -> 5,1@1\nB (red): 5,1@1 -> 3,3@2 -> 3,4@3 -> 5,5@4\nC (red): 5,1@1 -> 7,3@2 -> 7,4@3 -> 5,5@4\nD (red): 5,5@4 -> 5,6@5"))


;; non-splits
(let [or-state (run-for-or-state split-non-ambiguity)
      results (last (:results or-state))]
  (facts (:PercentEventsCorrect results) => (roughly 100.0)
         (:MeanTimeWithLabel results) => (roughly 4.00)
         (:MeanLabelCounts results) => (roughly 1.00)
         (paths-str (:paths (:problem-data (:ep-state or-state))))
         => "A (red): 4,3@0 -> 4,4@1 -> 5,5@2 -> 5,6@3\nB (blue): 4,3@0 -> 4,4@1 -> 3,5@2 -> 3,6@3"))
