(ns retrospect.problems.tracking.test.prepared
  (:use [clojure.test :only [deftest is]])
  (:use [retrospect.test.utils])
  (:use [retrospect.problems.tracking.problem :only
         [tracking-problem]])
  (:use [retrospect.problems.tracking.hypotheses :only
         [no-explainer-hyps]])
  (:use [retrospect.problems.tracking.prepared])
  (:use [retrospect.workspaces :only [get-hyps]])
  (:use [retrospect.colors])
  (:use [retrospect.epistemicstates :only [current-ep-state goto-ep-state]])
  (:use [retrospect.state]))

(deftest case-color-update
  (let [results (run tracking-problem (color-update))]
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
  (let [results (run tracking-problem (intersection-ambiguity))]
    (is (= 100.0 (:PEC (last results))))
    (is (= 0.0 (:PEW (last results))))
    (is (= 1.0 (:IDCorrect (last results))))
    (is (= 1.0 (:Acc (last results))))
    (is (= 1.0 (:Prec (last results))))
    (is (= 1.0 (:Recall (last results))))
    (is (= 1.0 (:Spec (last results)))))
  (let [results (run tracking-problem
                     (assoc-in (intersection-ambiguity)
                               [:params :MetaReasoning] "NoMetareasoning"))
        ep (current-ep-state (goto-ep-state (:ep-state-tree @or-state) "C"))
        no-explainers (:no-explainers (:final (:log (:workspace ep))))
        no-explainer-hyps (no-explainer-hyps no-explainers (:problem-data ep))]
    (is (= #{:location} (set (map :type no-explainer-hyps))))
    (is (= [{:x 3 :y 8 :time 2 :color red :entity (symbol "1")}
            {:x 3 :y 3 :time 2 :color blue :entity (symbol "2")}]
           (sort-by :entity (map #(assoc (:loc (:data %))
                                    :entity (:entity (:data %))
                                    :color (:color (:data %)))
                                 no-explainer-hyps))))
    (is (approx= 33.3 (:PEC (last results)) 0.1))
    (is (approx= 33.3 (:PEW (last results)) 0.1)))
  (let [results (run tracking-problem
                     (assoc-in (intersection-ambiguity)
                               [:params :MetaReasoning] "Batch1"))]
    (is (approx= 33.3 (:PEC (last results)) 0.1))
    (is (approx= 33.3 (:PEW (last results)) 0.1)))
  (let [results (run tracking-problem
                     (assoc-in (intersection-ambiguity)
                               [:params :MetaReasoning] "Batch2"))]
    (is (approx= 100.0 (:PEC (last results)) 0.1))
    (is (approx= 0.0 (:PEW (last results)) 0.1))
    (is (= 1.0 (:IDCorrect (last results)))))
  (let [results (run tracking-problem
                     (assoc-in (intersection-ambiguity)
                               [:params :MetaReasoning] "RetractNoExplainers"))]
    (is (approx= 100.0 (:PEC (last results)) 0.1))
    (is (approx= 0.0 (:PEW (last results)) 0.1))
    (is (= 1.0 (:IDCorrect (last results))))
    (is (= 1 (:MetaActivations (last results))))
    (is (= 1 (:MetaAccepted (last results))))))

(deftest case-random-bias-bug
  (let [results (run tracking-problem
                     (random-bias-bug-nometa))]
    (is (approx= 1.0 (:IDCorrect (last results)) 0.01)))
  (let [results (run tracking-problem
                     (assoc-in (random-bias-bug-nometa)
                               [:params :MetaReasoning] "RetractNoExplainers"))]
    (is (approx= 1.0 (:IDCorrect (last results)) 0.01)))
  (let [results (run tracking-problem
                     (assoc-in (random-bias-bug-nometa)
                               [:params :MetaReasoning] "BatchBeginning"))]
    (is (approx= 1.0 (:IDCorrect (last results)) 0.01))))
