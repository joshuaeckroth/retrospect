(ns retrospect.problems.tracking.test-prepared
  (:use [midje.sweet])
  (:use [retrospect.workspaces :only [last-id get-conf]])
  (:use [retrospect.epistemicstates :only
         [previous-ep-state flatten-ep-state-tree]])
  (:use [retrospect.onerun :only [init-one-run-state]])
  (:use [retrospect.problem :only [run-simulation run-simulation-step evaluate]])
  (:use [retrospect.problems.tracking.prepared])
  (:use [retrospect.problems.tracking.problem :only [tracking-problem]])
  (:use [retrospect.problems.tracking.hypotheses :only [paths-str]])
  (:use [retrospect.confidences])
  (:use [retrospect.random])
  (:require [clojure.set :as set]))

(defn newlines
  [& strs]
  (apply str (map #(format "%s\n" %) strs)))

(defn run-for-or-state
  [prepared]
  (set-seed 10)
  (let [params (:params prepared)
        meta-abduction (:MetaAbduction params)
        lazy (:Lazy params)
        sensors (:sensors prepared)
        truedata (:truedata prepared)
        or-state (init-one-run-state meta-abduction lazy sensors 10
                                     ((:gen-problem-data-fn tracking-problem)
                                      sensors params))]
    (binding [last-id 0]
      (loop [ors or-state]
        (if (>= (:time (:ep-state ors)) (:Steps params))
          ;; FIX THIS
          (evaluate tracking-problem truedata ors ors :MetaNone 0 params)
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
        or-state (init-one-run-state meta-abduction lazy sensors 10
                                     ((:gen-problem-data-fn tracking-problem)
                                      sensors params))]
    (binding [last-id 0]
      (run-simulation tracking-problem truedata or-state params false))))

;; color-updates
(let [or-state (run-for-or-state color-update)
      results (last (:results or-state))]
  (facts
   "Color update"
   (:PEC results) => (roughly 100.0)
   (:MTL results) => (roughly 3.0)
   (:MLC results) => (roughly 1.0)
   (paths-str (:paths (:problem-data (:ep-state or-state))))
   => "A (red): 9,5@0 -> 5,5@1 -> 2,5@2\n"))

(let [or-state (run-for-or-state color-update-2)
      results (last (:results or-state))]
  (facts
   "Color update 2"
   (:PEC results) => (roughly 100.0)
   (:MTL results) => (roughly 3.0)
   (:MLC results) => (roughly 1.0)
   (paths-str (:paths (:problem-data (:ep-state or-state))))
   => "A (red): 9,5@0 -> 5,5@1 -> 2,5@2\n"))

(let [results (run-for-results gray-in-range)]
  (facts "Grays did not cause a problem"
         (:PEC results) => (roughly 100.0)
         (:MTL results) => (roughly 3.0)
         (:MLC results) => (roughly 1.0)))

;; intersections
(let [results (run-for-results intersection-ambiguity)]
  (facts
   "Intersection ambiguity (with meta)"
   (:PEC results) => (roughly 100.0)
   (:MetaAbductions results) => 1
   (:MTL results) => (roughly 4.0)
   (:MLC results) => (roughly 1.0)))

(let [or-state (run-for-or-state intersection-ambiguity-nometa)
      results (last (:results or-state))]
  (facts
   "Intersection ambiguity (no meta)"
   (:PEC results) => (roughly 33.3 0.1)
   (:MetaAbductions results) => 0
   (:MTL results) => (roughly 1.3 0.1)
   (:MLC results) => (roughly 3.0)
   (paths-str (:paths (:problem-data (:ep-state or-state))))
   => (newlines "A (blue): 5,4@0 -> 4,4@1"
                "B (red): 5,7@0 -> 4,7@1"
                "C (blue): 2,8@2 -> 0,8@3"
                "D (red): 2,3@2 -> 0,3@3")))

(let [results (run-for-results intersection-ambiguity-nometa-allatonce)]
  (facts
   "Intersection ambiguity (no meta, all at once)"
   (:PEC results) => (roughly 100.0)
   (:MetaAbductions results) => 0
   (:MTL results) => (roughly 4.0)
   (:MLC results) => (roughly 1.0)))

;; splits
(let [or-state (run-for-or-state split-ambiguity)
      results (last (:results or-state))]
  (facts
   "Split ambiguity"
   (:PEC results) => (roughly 100.0)
   (:MTL results) => (roughly 2.0)
   (:MLC results) => (roughly 2.0)
   (paths-str (:paths (:problem-data (:ep-state or-state))))
   => (newlines "A* (red): 4,0@0 -> 4,2@1"
                "B (red): 4,2@1 -> 2,4@2 -> 2,6@3"
                "C (red): 4,2@1 -> 6,4@2 -> 6,6@3")))

(let [or-state (run-for-or-state split-ambiguity-2)
      results (last (:results or-state))]
  (facts
   "Split ambiguity 2"
   (:PEC results) => (roughly 100.0)
   (:MTL results) => (roughly 2.0)
   (:MLC results) => (roughly 2.0)
   (paths-str (:paths (:problem-data (:ep-state or-state))))
   => (newlines "A* (blue): 6,0@0 -> 6,1@1"
                "B* (red): 1,0@0 -> 1,1@1"
                "C (red): 1,1@1 -> 0,2@2 -> 0,3@3"
                "D (blue): 6,1@1 -> 5,2@2 -> 5,3@3"
                "E (red): 1,1@1 -> 2,2@2 -> 2,3@3"
                "F (blue): 6,1@1 -> 7,2@2 -> 7,3@3")))

;; merges
(let [or-state (run-for-or-state merge-ambiguity)
      results (last (:results or-state))]
  (facts
   "Merge ambiguity"
   (:PEC results) => (roughly 100.0)
   (:MTL results) => (roughly 1.66 0.1)
   (:MLC results) => (roughly 2.5)
   (paths-str (:paths (:problem-data (:ep-state or-state))))
   => (newlines "A* (red): 6,0@0 -> 6,2@1 -> 5,4@2"
                "B* (red): 4,0@0 -> 4,2@1 -> 5,4@2"
                "C (red): 5,4@2 -> 5,6@3")))

(let [or-state (run-for-or-state merge-ambiguity-2)
      results (last (:results or-state))]
  (facts
   "Merge ambiguity 2"
   (:PEC results) => (roughly 100.0)
   (:MTL results) => (roughly 1.66 0.1)
   (:MLC results) => (roughly 2.5)
   (paths-str (:paths (:problem-data (:ep-state or-state))))
   => (newlines "A* (blue): 9,0@0 -> 9,2@1 -> 8,4@2"
                "B* (red): 0,0@0 -> 0,2@1 -> 1,4@2"
                "C* (red): 2,0@0 -> 2,2@1 -> 1,4@2"
                "D* (blue): 7,0@0 -> 7,2@1 -> 8,4@2"
                "E (blue): 8,4@2 -> 8,6@3"
                "F (red): 1,4@2 -> 1,6@3")))

;; splits + merges
(let [or-state (run-for-or-state split-merge)
      results (last (:results or-state))]
  (facts
   "Split + merge"
   (:PEC results) => (roughly 100.0)
   (:MTL results) => (roughly 1.75)
   (:MLC results) => (roughly 3.50)
   (paths-str (:paths (:problem-data (:ep-state or-state))))
   => (newlines "A* (red): 5,0@0 -> 5,1@1"
                "B* (red): 5,1@1 -> 3,3@2 -> 3,4@3 -> 5,5@4"
                "C* (red): 5,1@1 -> 7,3@2 -> 7,4@3 -> 5,5@4"
                "D (red): 5,5@4 -> 5,6@5")))

(let [or-state (run-for-or-state split-merge-allatonce)
      results (last (:results or-state))]
  (facts
   "Split + merge (all at once)"
   (:PEC results) => (roughly 100.0)
   (:MTL results) => (roughly 1.75)
   (:MLC results) => (roughly 3.5)
   (paths-str (:paths (:problem-data (:ep-state or-state))))
   => (newlines "A* (red): 5,0@0 -> 5,1@1"
                "B* (red): 5,1@1 -> 3,3@2 -> 3,4@3 -> 5,5@4"
                "C* (red): 5,1@1 -> 7,3@2 -> 7,4@3 -> 5,5@4"
                "D (red): 5,5@4 -> 5,6@5")))

(let [results (run-for-results split-merge-twocolor)]
  (facts
   (:PEC results) =not=> (roughly 100.0 0.1)))

(let [results (run-for-results split-merge-twocolor-aao)]
  (facts
   (:PEC results) => (roughly 100.0 0.1)))

(let [or-state (run-for-or-state split-merge-gray)
      results (last (:results or-state))]
  (facts "Split-merge-gray"
         (:PEC results) => (roughly 100.0)
         (:MTL results) => (roughly 2.17 0.1)
         (:MLC results) => (roughly 3.0)
         (paths-str (:paths (:problem-data (:ep-state or-state))))
         => (newlines "A* (red): 3,0@0 -> 3,3@1 -> 5,4@2"
                      "B* (red): 7,0@0 -> 7,3@1 -> 5,4@2"
                      "C (blue): 5,0@0 -> 5,3@1 -> 5,4@2 -> 5,6@3 -> 7,8@4 -> 7,9@5"
                      "D* (red): 5,4@2 -> 5,6@3"
                      "E (red): 5,6@3 -> 5,8@4 -> 5,9@5"
                      "F (red): 5,6@3 -> 3,8@4 -> 3,9@5")))

;; non-splits
(let [or-state (run-for-or-state split-non-ambiguity)
      results (last (:results or-state))]
  (facts "Non-split"
   (:PEC results) => (roughly 100.0)
   (:MTL results) => (roughly 4.00)
   (:MLC results) => (roughly 1.00)
   (paths-str (:paths (:problem-data (:ep-state or-state))))
   => (newlines "A (blue): 4,3@0 -> 4,4@1 -> 3,5@2 -> 3,6@3"
                "B (red): 4,3@0 -> 4,4@1 -> 5,5@2 -> 5,6@3")))

;; rejected stuff is accepted bug
(let [or-state (run-for-or-state random-2)]
  (facts "Rejected are accepted bug"
         (apply set/union (map #(let [final (:final (:log (:workspace %)))
                                      accepted (set (map :id (:accepted final)))
                                      rejected (set (map :id (:rejected final)))]
                                  (set/intersection accepted rejected))
                               (flatten-ep-state-tree (:ep-state-tree or-state))))
         => (just #{})))

(let [results (run-for-results random-5)
      results-meta (run-for-results (assoc-in random-5 [:params :MetaAbduction] true))]
  (facts (:Unexplained results) => 1
         (:Unexplained results-meta) => 0))

(let [results (run-for-results random-6)
      results-aao (run-for-results (assoc-in random-6 [:params :StepsBetween] 20))
      results-meta (run-for-results (assoc-in random-6 [:params :MetaAbduction] true))]
  (facts (:Unexplained results) => 3
         (:Unexplained results-aao) => 0
         (:Unexplained results-meta) => 0))
