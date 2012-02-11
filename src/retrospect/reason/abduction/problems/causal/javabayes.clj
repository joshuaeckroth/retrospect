(ns retrospect.problems.causal.javabayes
  (:import (javabayes.InferenceGraphs InferenceGraph))
  (:import (javabayes.BayesianNetworks BayesNet))
  (:import (javabayes.BayesianInferences Explanation))
  (:import (javabayes.QuasiBayesianInferences QBInference QBExpectation))
  (:import (java.io ByteArrayInputStream))
  (:use [clojure.contrib.seq :only [find-first]])
  (:use [loom.graph :only [digraph add-edges nodes incoming]])
  (:use [loom.attr :only [attr add-attr]])
  (:use [retrospect.state]))

(defn load-bayesnet
  [file]
  (.get_bayes_net (InferenceGraph. (str @datadir "/causal/" file))))

(defn build-bayesnet
  [network]
  (let [ns (sort (nodes network))
        vars-str (fn [n]
                   (let [values (attr network n :values)]
                     (format "variable \"%s\" { %s }\n" n
                             (format "type discrete[%d] { %s };"
                                     (count values)
                                     (apply str (map #(format "\"%s\" " %)
                                                     values))))))
        probs-str (fn [n]
                    (let [probs (attr network n :probs)
                          vars (concat [n] (sort (incoming network n)))]
                      (format "probability ( %s ) { table %s; }\n"
                              (apply str (map #(format "\"%s\" " %) vars))
                              (apply str (map #(format "%f " %) probs)))))
        bif (format "network \"network\" {}\n %s %s"
                    (apply str (map vars-str ns))
                    (apply str (map probs-str ns)))]
    (BayesNet. (ByteArrayInputStream. (.getBytes bif)))))

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
              (-> g-with-values
                  (add-attr node :id node)
                  (add-attr node :label node)
                  (add-attr node :probs
                            (vec (map (fn [i] (.get_value f i))
                                      (range 0 (.number_values f))))))))
          (digraph) (.get_probability_functions bayesnet)))

(defn get-var
  [bn node]
  (find-first (fn [v] (= node (.get_name v)))
              (.get_probability_variables bn)))

(defn observe
  [bn node value]
  (.set_observed_value (get-var bn node) value))

(defn observe-seq
  [bn obs-seq]
  (doseq [[n v] (filter not-empty obs-seq)]
    (observe bn n v)))

(defn unobserve
  [bn node]
  (.set_invalid_observed_index (get-var bn node)))

(defn unobserve-all
  [bn]
  (doseq [n (map #(.get_name %) (.get_probability_variables bn))]
    (unobserve bn n)))

(defn get-observed
  [bn node]
  (let [v (get-var bn node)
        i (.get_observed_index v)]
    (if (>= i 0) (.get_value v i))))

(defn get-posterior-marginal
  ([bn node]
     (let [inf (QBInference. bn false)]
       (.inference inf node)
       (let [result (.get_result inf)
             names (.get_values (first (.get_variables result)))
             vals (.get_values result)]
         (reduce (fn [m i] (assoc m (nth names i) (nth vals i)))
                 {} (range (count names))))))
  ([bn node val]
     (get (get-posterior-marginal bn node) val)))

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
    (.full_explanation expl)
    (let [bp (.backward_pointers (.bucket_tree expl))]
      (reduce (fn [m i]
                (if (= -1 (nth bp i)) m
                    (let [v (.get_probability_variable bn i)]
                      (assoc m (.get_name v) (.get_value v (nth bp i))))))
              {} (range (count bp))))))
