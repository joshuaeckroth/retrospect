(ns retrospect.problems.causal.truedata
  (:use [retrospect.random])
  (:use [loom.graph :only [digraph nodes edges]])
  (:use [loom.attr :only [attr]]))

(defn observed-seq
  [network]
  (let [observable-nodes (filter #(nil? (attr network % :apriori))
                                 (nodes network))]))

(defn generate-truedata
  []
  {:network (digraph) :observed-seq []})
