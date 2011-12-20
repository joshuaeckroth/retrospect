(ns retrospect.problems.causal.javabayes
  (:import (javabayes.InferenceGraphs InferenceGraph))
  (:import (javabayes.BayesianNetworks DiscreteVariable DiscreteFunction BayesNet))
  (:import (javabayes.BayesianInferences Explanation))
  (:import (javabayes.QuasiBayesianInferences QBInference QBExpectation))
  (:use [clojure.contrib.seq :only [find-first]])
  (:use [loom.graph :only [digraph add-edges nodes incoming]])
  (:use [loom.attr :only [attr add-attr]]))

(defn load-bayesnet
  [file]
  (.get_bayes_net (InferenceGraph. file)))

(defn build-bayesnet
  [network]
  (let [vars (reduce (fn [m n] (assoc m n (DiscreteVariable.
                                           n 0
                                           (into-array (attr network n :values)))))
                     {} (nodes network))
        funcs (map (fn [n]
                     (let [parents (sort (incoming network n))
                           probs (attr network n :probs)]
                       (DiscreteFunction.
                        (into-array DiscreteVariable (concat [(get vars n)]
                                                             (map #(get vars %) parents)))
                        (double-array probs))))
                   (nodes network))]
    (doto (BayesNet.)
      (.add (into-array DiscreteVariable (vals vars)))
      (.add (into-array DiscreteFunction funcs)))))

(defn build-network
  [bayesnet]
  (reduce (fn [g f]
            (let [nvars (.number_variables f)
                  vars (.get_variables f)
                  node (.get_name (first vars))
                  parents (map #(.get_name %) (rest vars))
                  g-with-edges (apply add-edges g (map (fn [p] [p node]) parents))
                  g-with-values (reduce (fn [g v]
                                          (add-attr g (.get_name v)
                                                    :values (vec (.get_values v))))
                                        g-with-edges vars)]
              (add-attr g-with-values node :probs
                        (vec (map (fn [i] (.get_value f i))
                                  (range 0 (.number_values f)))))))
          (digraph) (.get_probabilities bayesnet)))

(defn get-var
  [bn node]
  (find-first (fn [v] (= (str node) (.get_name v)))
              (.get_probability_variables bn)))

(defn observe
  [bn node value]
  (.set_observed_value (get-var bn node) value))

(defn unobserve
  [bn node]
  (.set_invalid_observed_index (get-var bn node)))

(defn get-observed
  [bn node]
  (let [v (get-var bn node)
        i (.get_observed_index v)]
    (if (>= i 0) (.get_value v i))))

(defn get-posterior-marginal
  [bn node]
  (let [inf (QBInference. bn false)]
    (.inference inf node)
    (let [result (.get_result inf)
          names (.get_values (first (.get_variables result)))
          vals (.get_values result)]
      (reduce (fn [m i] (assoc m (nth names i) (nth vals i)))
              {} (range (count names))))))

(defn get-expectation
  [bn node]
  (let [exp (QBExpectation. bn false)]
    (.expectation exp node)
    (let [results (.get_results exp)]
      (seq results))))

(defn set-explanatory
  [bn node]
  (.set_explanation_value (get-var bn node) 0))

(defn get-explanation
  [bn]
  (let [expl (Explanation. bn)]
    (.explanation expl)
    (let [bp (.backward_pointers (.bucket_tree expl))]
      (reduce (fn [m i]
                (if (= -1 (nth bp i)) m
                    (let [v (.get_probability_variable bn i)]
                      (assoc m (.get_name v) (.get_value v (nth bp i))))))
              {} (range (count bp))))))
