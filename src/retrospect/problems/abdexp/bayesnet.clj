(ns retrospect.problems.abdexp.bayesnet
  (:import (org.openmarkov.io.probmodel PGMXReader PGMXWriter))
  (:import (org.openmarkov.inference.variableElimination VariableElimination))
  (:import (org.openmarkov.core.model.network
            EvidenceCase NodeType ProbNet State Variable))
  (:import (org.openmarkov.core.model.network.potential
            Potential PotentialRole TablePotential))
  (:use [retrospect.problems.abdexp.expgraph]))

(defn load-pgmx
  [file]
  (let [reader (PGMXReader.)
        bayesnet (.getProbNet (.loadProbNet reader file))
        evidence (EvidenceCase.)
        inference (doto (VariableElimination. bayesnet)
                    (.setPreResolutionEvidence evidence))]
    {:bayesnet bayesnet
     :inference inference
     :evidence evidence}))

(defn gen-parent-var-idxs
  [var-idxs]
  (if (empty? var-idxs) [[]]
      (apply concat
             (for [var-idx-seq (first var-idxs)]
               (map #(conj % var-idx-seq)
                  (gen-parent-combinations (rest var-idxs)))))))

;; TODO: support conflicts
(defn build-bayesnet
  [expgraph]
  (let [bn (ProbNet.)
        vars (reduce (fn [m v]
                  (let [vals (sort (values expgraph v))
                        var (Variable. (str v) (into-array State (map #(State. %) vals)))]
                    (assoc m v var)))
                {} (sort (vertices expgraph)))]
    (doseq [v (keys vars)]
      (.addVariable bn (get vars v) NodeType/CHANCE))
    (doseq [v2 (keys vars)]
      (doseq [v1 (sort (explainers expgraph v2))]
        (.addLink bn (get vars v1) (get vars v2) true)))
    (doseq [v2 (keys vars)]
      (let [parent-vars (java.util.ArrayList.
                         (concat [(get vars v2)]
                                 (map #(get vars %) (sort (explainers expgraph v2)))))
            potential (TablePotential. parent-vars
                                       PotentialRole/CONDITIONAL_PROBABILITY
                                       (double-array (:table (probs expgraph v2))))]
        (.addPotential bn potential)))
    (let [evidence (EvidenceCase.)
          inference (doto (VariableElimination. bn)
                      (.setPreResolutionEvidence evidence))]
      {:bayesnet bn
       :inference inference
       :evidence evidence})))

(defn save-bayesnet
  [bn filename]
  (let [writer (PGMXWriter.)]
    (.writeProbNet writer filename bn)))

(defn get-posterior
  ([inf vertex])
  ([bn vertex value]
     (let [var (.getVariable (:bayesnet bn) vertex)
           state (.getStateIndex var value)]
       (nth (.getValues (get (.getProbsAndUtilities (:inference bn)) var)) state))))

(defn observe
  [bn vertex value]
  (.addFinding (:evidence bn) (:bayesnet bn) vertex value))

(defn observe-seq
  [bn obs-seq]
  (doseq [[v val] (filter not-empty obs-seq)]
    (observe bn v val)))

(defn unobserve
  [bn vertex]
  (.removeFinding (:evidence bn) vertex))

(defn unobserve-all
  [bn]
  (doseq [var (.getVariables (:evidence bn))]
    (.removeFinding (:evidence bn) var)))

