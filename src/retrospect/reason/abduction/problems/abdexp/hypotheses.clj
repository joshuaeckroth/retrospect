(ns retrospect.reason.abduction.problems.abdexp.hypotheses
  (:require [clojure.set :as set])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.reason.abduction.workspace :only [new-hyp]])
  (:use [retrospect.problems.abdexp.javabayes])
  (:use [retrospect.problems.abdexp.expgraph])
  (:require [retrospect.state :as state]))

(defn hyps-conflict?
  [expgraph hyp1 hyp2]
  (and (not= (:id hyp1) (:id hyp2))
       (or (and (= (:vertex hyp1) (:vertex hyp2))
                (not= (:value hyp1) (:value hyp2)))
           (conflicts? expgraph
                       [(:vertex hyp1) (:value hyp1)]
                       [(:vertex hyp2) (:value hyp2)]))))

(defn make-hyps
  [expgraph]
  (loop [hyps {}
         vertices (sorted-by-dep expgraph)]
    (if (empty? vertices)
      (doall
       (mapcat (fn [v] (map (fn [val] (get hyps [v val])) (values expgraph v)))
             (sorted-by-dep expgraph)))
      (let [v (first vertices)
            vals (values expgraph v)]
        (recur
         (reduce (fn [m val]
              (assoc m [v val]
                     (new-hyp "Expl" :expl :expl
                              (score expgraph v val)
                              (not-empty (explainers expgraph v))
                              #(hyps-conflict? expgraph %1 %2)
                              (map #(:contents (get hyps %))
                                 (mapcat (fn [v2] (map (fn [val2] [v2 val2])
                                                  (values expgraph v2)))
                                       (explains expgraph v)))
                              (format "%s=%s" v val) (format "%s=%s" v val)
                              {:vertex v :value val})))
            hyps (values expgraph v))
         (rest vertices))))))

(defn generate-kb
  "This function introduces the whole expgraph so that it is ready to
   use when reasoning starts."
  [training]
  (conj (make-hyps (:expgraph training))
        (new-hyp "KB" :kb :kb 1.0 false nil [] "" ""
                 {:expgraph (:expgraph training) :bayesnet (:bayesnet training)})))

(defn get-kb
  [accepted lookup-hyp]
  (first (filter :expgraph (map lookup-hyp (get accepted :kb)))))

;; hyps are identified by their # (or category?); update-kb thinks the
;; accepted ones are more common; adjusts the apriori scores; maybe
;; even decides the apriori scores
(defn update-kb
  [accepted unexplained hypotheses lookup-hyp]
  (map lookup-hyp (get accepted :kb)))

(defn make-sensor-hyps
  "Pick out the hyps that have been observed."
  [sensors time-prev time-now accepted all-hyps lookup-hyp]
  (if (= time-prev time-now) []
      (let [observed (set (mapcat #(sensed-at (first sensors) %)
                                  (range (inc time-now))))]
        (filter (fn [hyp] (observed [(:vertex hyp) (:value hyp)]))
           (map lookup-hyp (:all all-hyps))))))

(defn explanatory?-delta
  "Is P(vertex2=val2|vertex1=val1) > P(vertex2=val2)? If so,
   vertex1=val1 is explanatory (according to Gardenfors)."
  [bn vertex1 val1 vertex2 val2 observed]
  (unobserve-all bn)
  (observe-seq bn (filter #(not= vertex2 (first %)) observed))
  (let [prior (get-posterior-marginal bn vertex2 val2)]
    (observe bn vertex1 val1)
    (let [posterior (get-posterior-marginal bn vertex2 val2)]
      (< 0.0 (- posterior prior)))))

(defn explanatory?
  [bn vertex1 val1 vertex2 val2 observed]
  (cond (= "gardenfors-delta" (:ExplanatoryDef state/params))
        (explanatory?-delta bn vertex1 val1 vertex2 val2 observed)
        :else true))

(defn hypothesize
  [unexp-hyps accepted lookup-hyp time-now]
  [])

