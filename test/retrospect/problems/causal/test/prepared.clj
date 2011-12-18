(ns retrospect.problems.causal.test.prepared
  (:use [clojure.test :only [deftest is]])
  (:use [retrospect.test.utils])
  (:use [retrospect.problems.causal.problem :only [causal-problem]])
  (:use [retrospect.problems.causal.prepared]))

(deftest case-abc
  (let [results (run causal-problem (abc))]
    (is (= 1 1))))

(deftest case-simple-medium
  (let [results (run causal-problem (simple-medium))]
    (is (= 1 1))))