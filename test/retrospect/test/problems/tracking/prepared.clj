(ns retrospect.test.problems.tracking.prepared
  (:use [clojure.test :only [deftest is]])
  (:use [retrospect.test.utils])
  (:use [retrospect.problems.tracking.problem :only
         [generate-problem-data tracking-problem]])
  (:use [retrospect.problems.tracking.prepared])
  (:use [retrospect.onerun :only [init-one-run-state]])
  (:use [retrospect.problem :only [run-simulation]])
  (:use [retrospect.workspaces :only [last-id get-hyps]])
  (:use [retrospect.random :only [rgen new-seed]])
  (:use [retrospect.epistemicstates :only [current-ep-state goto-ep-state]])
  (:use [retrospect.state]))

(defn run
  [{:keys [params sensors truedata]}]
  (dosync
   (alter retrospect.state/datadir (constantly "data"))
   (alter retrospect.state/problem (constantly tracking-problem)))
  (binding [rgen (new-seed (:Seed params))
            last-id 0
            retrospect.state/params params]
    (let [problem-data (generate-problem-data truedata sensors)
          or-state (init-one-run-state sensors problem-data)]
      (run-simulation truedata or-state false))))

(deftest case-color-update
  (let [results (run (color-update))]
    (is (= 1 (count results)))
    (is (= 100.0 (:PEC (first results))))
    (is (= 0.0 (:PEW (first results))))
    (is (= 1.0 (:IDCorrect (first results))))
    (is (= 1.0 (:Acc (first results))))
    (is (= 1.0 (:Prec (first results))))
    (is (= 1.0 (:Recall (first results))))
    (is (= 1.0 (:Spec (first results))))))

(deftest case-intersection-ambiguity
  ;; default is BatchBeginning
  (let [results (run (intersection-ambiguity))]
    (is (= 100.0 (:PEC (last results))))
    (is (= 0.0 (:PEW (last results))))
    (is (= 1.0 (:IDCorrect (last results))))
    (is (= 1.0 (:Acc (last results))))
    (is (= 1.0 (:Prec (last results))))
    (is (= 1.0 (:Recall (last results))))
    (is (= 1.0 (:Spec (last results)))))
  (let [results (run (assoc-in (intersection-ambiguity)
                               [:params :MetaReasoning] "NoMetareasoning"))]
    (is (approx= 33.3 (:PEC (last results)) 0.1))
    (is (approx= 33.3 (:PEW (last results)) 0.1)))
  (let [results (run (assoc-in (intersection-ambiguity)
                               [:params :MetaReasoning] "Batch1"))]
    (is (approx= 33.3 (:PEC (last results)) 0.1))
    (is (approx= 33.3 (:PEW (last results)) 0.1)))
  (let [results (run (assoc-in (intersection-ambiguity)
                               [:params :MetaReasoning] "Batch2"))]
    (is (approx= 100.0 (:PEC (last results)) 0.1))
    (is (approx= 0.0 (:PEW (last results)) 0.1))))
