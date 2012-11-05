(ns retrospect.problems.abdexp.javabayes
  (:import (javabayes.InferenceGraphs InferenceGraph))
  (:import (javabayes.BayesianNetworks BayesNet))
  (:import (javabayes.BayesianInferences Explanation))
  (:import (javabayes.QuasiBayesianInferences QBInference QBExpectation))
  (:import (java.io ByteArrayInputStream))
  (:use [clojure.contrib.seq :only [find-first]])
  (:use [loom.graph :only [digraph add-edges nodes incoming]])
  (:use [loom.attr :only [attr add-attr]])
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [retrospect.state]))

(defn load-bayesnet
  [file]
  (.get_bayes_net (InferenceGraph. (str @datadir "/causal/" file))))

(defn build-bayesnet
  [expgraph]
  (let [vs (sort (vertices expgraph))
        vars-str (fn [v]
                   (let [vals (values expgraph v)]
                     (format "variable \"%s\" { %s }\n" v
                             (format "type discrete[%d] { %s };"
                                     (count vals)
                                     (apply str (map #(format "\"%s\" " %) vals))))))
        probs-str (fn [v]
                    (let [vars (concat [v] (sort (explainers expgraph v)))]
                      (format "probability ( %s ) { table %s; }\n"
                              (apply str (map #(format "\"%s\" " %) vars))
                              (apply str (map #(format "%f " %) (:table (probs expgraph v)))))))
        bif (format "network \"network\" {}\n %s %s"
                    (apply str (map vars-str vs))
                    (apply str (map probs-str vs)))]
    (BayesNet. (ByteArrayInputStream. (.getBytes bif)))))

(defn get-posterior-marginal
  ([bn vertex]
     (let [inf (QBInference. bn false)]
       (.inference inf vertex)
       (let [result (.get_result inf)
             names (.get_values (first (.get_variables result)))
             vals (.get_values result)]
         (reduce (fn [m i] (assoc m (nth names i) (nth vals i)))
                 {} (range (count names))))))
  ([bn vertex val]
     (get (get-posterior-marginal bn vertex) val)))

(defn get-expectation
  [bn vertex]
  (let [exp (QBExpectation. bn false)]
    (.expectation exp vertex)
    (let [results (.get_results exp)]
      (seq results))))

(defn build-expgraph
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
         (-> g-with-values
            (add-attr node :probs
                      (vec (map (fn [i] (.get_value f i))
                              (range (.number_values f)))))
            (add-attr node :scores (get-posterior-marginal bayesnet node)))))
     (digraph) (.get_probability_functions bayesnet)))

(defn get-var
  [bn vertex]
  (find-first (fn [v] (= vertex (.get_name v)))
              (.get_probability_variables bn)))

(defn observe
  [bn vertex value]
  (.set_observed_value (get-var bn vertex) value))

(defn observe-seq
  [bn obs-seq]
  (doseq [[n v] (filter not-empty obs-seq)]
    (observe bn n v)))

(defn unobserve
  [bn vertex]
  (.set_invalid_observed_index (get-var bn vertex)))

(defn unobserve-all
  [bn]
  (doseq [n (map #(.get_name %) (.get_probability_variables bn))]
    (unobserve bn n)))

(defn get-observed
  [bn vertex]
  (let [v (get-var bn vertex)
        i (.get_observed_index v)]
    (if (>= i 0) (.get_value v i))))

(defn set-explanatory
  [bn vertex]
  (.set_explanation_value (get-var bn vertex) 0))

(defn get-explanation
  [bn]
  (let [expl (Explanation. bn)]
    (.full_explanation expl)
    (let [bp (.backward_pointers (.bucket_tree expl))]
      (reduce (fn [m i]
                (if (= -1 (nth bp i)) m
                    (let [v (.get_probability_variable bn i)]
                      (assoc m (.get_name v) (.get_value v (nth bp i))))))
              {} (range (count bp))))))
