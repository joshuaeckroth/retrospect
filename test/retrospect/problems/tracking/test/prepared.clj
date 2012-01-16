(ns retrospect.problems.tracking.test.prepared
  (:use [clojure.test :only [deftest is]])
  (:require [clojure.set :as set])
  (:use [clojure.contrib.combinatorics :only [combinations]])
  (:use [loom.graph :only [nodes incoming]])
  (:use [loom.alg :only [pre-traverse scc]])
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

(deftest case-gray-in-range
  (let [results (run tracking-problem (gray-in-range))]
    (is (= 2 (count results)))
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
  ;; Batch1 is not enough to fix the problem
  (let [results (run tracking-problem
                     (assoc-in (intersection-ambiguity)
                               [:params :MetaReasoning] "Batch1"))]
    (is (approx= 33.3 (:PEC (last results)) 0.1))
    (is (approx= 33.3 (:PEW (last results)) 0.1)))
  ;; Batch2 is enough to fix the problem
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

(deftest case-intersection-continued-ambiguity
  (let [results (run tracking-problem (intersection-continued-ambiguity))]
    (is (= 2 2))))

(defn update-params
  [prepared key val]
  (assoc-in prepared [:params key] val))

(deftest case-random-bias-bug
  (let [case-nometa (random-bias-bug-nometa)
        case-retract (update-params case-nometa :MetaReasoning
                                    "RetractNoExplainers")
        case-batch (update-params case-nometa :MetaReasoning
                                  "BatchBeginning")
        results-nometa (run tracking-problem case-nometa)
        results-retract (run tracking-problem case-retract)
        results-batch (run tracking-problem case-batch)]
    (is (approx= 1.0 (:IDCorrect (last results-nometa)) 0.01))
    (is (approx= 1.0 (:IDCorrect (last results-retract)) 0.01))
    (is (approx= 1.0 (:IDCorrect (last results-batch)) 0.01))))

(deftest case-color-matching-bug
  (let [results (run tracking-problem (color-matching-bug))]
    (is (approx= 0.83 (:IDCorrect (last results)) 0.1))))
