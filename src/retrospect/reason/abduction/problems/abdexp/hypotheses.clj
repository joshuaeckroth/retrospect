(ns retrospect.reason.abduction.problems.abdexp.hypotheses
  (:require [clojure.set :as set])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.reason.abduction.workspace :only [new-hyp]])
  (:use [retrospect.problems.abdexp.javabayes])
  (:use [retrospect.problems.abdexp.expgraph])
  (:require [retrospect.state :as state]))

(defn generate-kb
  [training]
  [(new-hyp "KB" :kb :kb 1.0 false nil [] "" ""
            {:expgraph (:expgraph training) :bayesnet (:bayesnet training)})])

(defn get-kb
  [accepted lookup-hyp]
  (first (filter :expgraph (map lookup-hyp (get accepted :kb)))))

;; hyps are identified by their # (or category?); update-kb thinks the
;; accepted ones are more common; adjusts the apriori scores; maybe
;; even decides the apriori scores
(defn update-kb
  [accepted unexplained hypotheses lookup-hyp]
  (map lookup-hyp (get accepted :kb)))

(defn hyps-conflict?
  [expgraph hyp1 hyp2]
  (and (not= (:id hyp1) (:id hyp2))
       (or (and (= :ignore (:subtype hyp1))
                (= :observation (:subtype hyp2))
                (= (:vertex hyp1) (:vertex hyp2)))
           (and (= :ignore (:subtype hyp2))
                (= :observation (:subtype hyp1))
                (= (:vertex hyp2) (:vertex hyp1)))
           (conflicts? expgraph (:vertex hyp1) (:vertex hyp2)))))

(defn make-sensor-hyps
  [sensors time-prev time-now accepted lookup-hyp]
  (if (= time-prev time-now) []
      (let [kb (get-kb accepted lookup-hyp)
            expgraph (:expgraph kb)
            prev-hyps (map lookup-hyp (:all accepted))
            observed (reduce set/union (map #(sensed-at (first sensors) %)
                                     (range (inc time-now))))]
        (loop [hyps (zipmap (map :vertex prev-hyps) prev-hyps)
               vertices observed]
          (if (empty? vertices)
            (filter #(= :observation (:type %)) (vals hyps))
            (let [[v value] (first vertices)]
              (recur
               (assoc hyps v (new-hyp "Obs" :observation :observation
                                      1.0 true
                                      #(hyps-conflict? expgraph %1 %2)
                                      (map #(:contents (get hyps %)) (explains expgraph v))
                                      (format "%s=%s" v value) (format "%s=%s" v value)
                                      {:vertex v :value value}))
               (rest vertices))))))))

(defn explanatory?-delta
  "Is P(node2=val2|node1=val1) > P(node2=val2)? If so, node1=val1 is
   explanatory (according to Gardenfors)."
  [bn node1 val1 node2 val2 observed]
  (unobserve-all bn)
  (observe-seq bn (filter #(not= node2 (first %)) observed))
  (let [prior (get-posterior-marginal bn node2 val2)]
    (observe bn node1 val1)
    (let [posterior (get-posterior-marginal bn node2 val2)]
      (< 0.0 (- posterior prior)))))

(defn explanatory?
  [bn node1 val1 node2 val2 observed]
  (explanatory?-delta bn node1 val1 node2 val2 observed))

(defn find-explanatory-assignments
  [bn expgraph implicated observed]
  (map (fn [[[n1 v1] [n2 v2]]]
       {:node1 n1 :val1 v1
        :node2 n2 :val2 v2})
     (filter (fn [[[n1 v1] [n2 v2]]]
          (explanatory? bn n1 v1 n2 v2 observed))
        (mapcat (fn [n]
                  (let [effects (set (explains expgraph n))
                        effects-no-obs (set/difference effects (set (map first observed)))
                        effects-vals (mapcat (fn [e] (map (fn [v] [e v])
                                                       (values expgraph e)))
                                             effects-no-obs)
                        effects-obs-vals (concat effects-vals
                                                 (filter #(effects (first %)) observed))
                        vals (values expgraph n)]
                    (mapcat (fn [nv] (map (fn [ev] [nv ev]) effects-obs-vals))
                            (map (fn [v] [n v]) vals))))
                implicated))))

(defn make-explanation-hyps
  [expgraph explanatory observed-hyps]
  (let [get-hyps (fn [m] (map (fn [k] (get m k))
                           (concat
                            ;; for "self" hyps
                            (map (fn [{:keys [vertex value]}]
                                 [vertex value vertex value])
                               (vals observed-hyps))
                            ;; for normal hyps
                            (map (fn [{:keys [node1 val1 node2 val2]}]
                                 [node1 val1 node2 val2])
                               explanatory))))
        hyps-no-explains
        (reduce (fn [m h] (let [{:keys [vertex vertex2 value value2]} h]
                      (-> m (update-in [[vertex value]] conj h)
                         (assoc [vertex value vertex2 value2] h))))
           {} (concat
               ;; no-causal-commitment explainers: the event "just happened"
               (map (fn [{:keys [vertex value]}]
                    (new-hyp "ExplSelf" :expl :expl (score expgraph vertex value)
                             false ;; does not need to be explained
                             #(hyps-conflict? expgraph %1 %2)
                             [] ;; explains -- filled in later
                             (format "%s=%s just happend" vertex value)
                             (format "%s=%s just happend" vertex value)
                             {:vertex vertex :value value
                              :vertex2 vertex :value2 value}))
                  (vals observed-hyps))
               ;; causal-commitment explainers: we can say why it happened
               (map (fn [{:keys [node1 val1 node2 val2 delta]}]
                    (new-hyp "Expl" :expl :expl (score expgraph node1 val1)
                             (not-empty (explainers expgraph node1))
                             #(hyps-conflict? expgraph %1 %2)
                             [] ;; explains -- filled in later
                             (format "%s=%s explains %s=%s" node1 val1 node2 val2)
                             (format "%s=%s explains %s=%s" node1 val1 node2 val2)
                             {:vertex node1 :value val1
                              :vertex2 node2 :value2 val2}))
                  explanatory)))
        hyps-explains
        (reduce (fn [m h]
             (let [{:keys [vertex value vertex2 value2]} h
                   obs-hyp (get observed-hyps [vertex2 value2])
                   explains (or (get m [vertex2 value2]) [])
                   obs-expl (if obs-hyp (conj explains obs-hyp)
                                explains)
                   h2 (assoc h :explains (map :contents obs-expl))]
               (if (empty? obs-expl) m
                   (-> m (update-in [[vertex value]] conj h2)
                      (assoc [vertex value vertex2 value2] h2)))))
           {} (get-hyps hyps-no-explains))]
    (filter identity (get-hyps hyps-explains))))

(defn hypothesize
  [unexp-hyps accepted lookup-hyp time-now]
  (let [kb (get-kb accepted lookup-hyp)
        expgraph (:expgraph kb)
        bn (:bayesnet kb)
        sensor-hyps (filter #(= :observation (:type %)) unexp-hyps)
        ;; sensor hyps plus previously-accepted explanations are "observed"
        observed-hyps (reduce (fn [m hyp] (assoc m [(:vertex hyp) (:value hyp)] hyp))
                         {} (concat sensor-hyps (map lookup-hyp (get accepted :expl))))
        ;; just the vertices of the observed
        observed-set (set (map first (keys observed-hyps)))
        observed-unexplained (filter (fn [[n val]]
                                  (let [expl (set (explainers expgraph n))]
                                    ;; an observed node/value is
                                    ;; unexplained if none of its
                                    ;; accepted immediate parents are
                                    ;; explanatory
                                    (not-any? (fn [[n2 val2]]
                                                (and (expl n2)
                                                     (explanatory? bn n2 val2 n val (keys observed-hyps)))))))
                                (keys observed-hyps))
        implicated (filter (fn [v] (not (observed-set v)))
                      (sorted-by-dep expgraph observed-set))
        explanatory (find-explanatory-assignments bn expgraph implicated (keys observed-hyps))]
    (make-explanation-hyps expgraph explanatory observed-hyps)))

