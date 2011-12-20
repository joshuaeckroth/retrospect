(ns retrospect.problems.causal.test.prepared
  (:use [clojure.test :only [deftest is]])
  (:use [retrospect.test.utils])
  (:use [retrospect.state])
  (:use [retrospect.problems.causal.problem :only [causal-problem]])
  (:use [retrospect.problems.causal.prepared]))

(deftest case-abc
  (let [results (run causal-problem (abc))]
    (is (= 1 1))))

(deftest case-simple-medium
  (let [results (run causal-problem (simple-medium))]
    (is (approx= 100.0 (:UnexplainedPct (last results)) 0.01)))
  (let [results (run causal-problem (assoc-in (simple-medium)
                                              [:params :StepsBetween] 2))]
    (is (approx= 0.0 (:UnexplainedPct (last results)) 0.01)))
  (let [results (run causal-problem (assoc-in (simple-medium)
                                              [:params :MetaReasoning] "BatchBeginning"))]
    (is (approx= 0.0 (:UnexplainedPct (last results)) 0.01)))
  (let [results (run causal-problem (assoc-in (simple-medium)
                                              [:params :MetaReasoning] "Batch1"))]
    (is (approx= 0.0 (:UnexplainedPct (last results)) 0.01)))
  (let [results (run causal-problem (assoc-in (simple-medium)
                                              [:params :MetaReasoning] "Batch2"))]
    (is (approx= 0.0 (:UnexplainedPct (last results)) 0.01)))
  (let [results (run causal-problem (assoc-in (simple-medium)
                                              [:params :MetaReasoning] "Batch3"))]
    (is (approx= 0.0 (:UnexplainedPct (last results)) 0.01))))

(deftest case-alarm
  (dosync (alter datadir (constantly "data")))
  (let [results (run causal-problem (alarm))]
    (is (= 1 1))))