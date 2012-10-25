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
           (conflicts? expgraph
                       [(:vertex hyp1) (:value hyp1)]
                       [(:vertex hyp2) (:value hyp2)]))))

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

(defn find-explanatory-assignments
  [bn expgraph implicated observed]
  (map (fn [[[vertex1 val1] [vertex2 val2]]]
       {:vertex1 vertex1 :val1 val1
        :vertex2 vertex2 :val2 val2})
     (filter (fn [[[vertex1 val1] [vertex2 val2]]]
          (explanatory? bn vertex1 val1 vertex2 val2 observed))
        (mapcat (fn [vertex]
                  (let [effects (set (explains expgraph vertex))
                        effects-no-obs (set/difference effects (set (map first observed)))
                        effects-vals (mapcat (fn [e] (map (fn [v] [e v])
                                                       (values expgraph e)))
                                             effects-no-obs)
                        effects-obs-vals (concat effects-vals
                                                 (filter #(effects (first %)) observed))
                        vals (values expgraph vertex)]
                    (mapcat (fn [v] (map (fn [ev] [v ev]) effects-obs-vals))
                            (map (fn [val] [vertex val]) vals))))
                implicated))))

(defn make-explanation-hyps
  [expgraph explanatory observed-hyps]
  (let [get-hyps (fn [m] (map #(get m %)
                           (map (fn [{:keys [vertex1 val1 vertex2 val2]}]
                                [vertex1 val1 vertex2 val2])
                              explanatory)))
        hyps-no-explains
        (reduce (fn [m h] (let [{:keys [vertex vertex2 value value2]} h]
                      (-> m (update-in [[vertex value]] conj h)
                         (assoc [vertex value vertex2 value2] h))))
           {} (map (fn [{:keys [vertex1 val1 vertex2 val2 delta]}]
                   {:vertex vertex1 :value val1 :vertex2 vertex2 :value2 val2
                    :hyp (new-hyp "Expl" :expl :expl (score expgraph vertex1 val1)
                                  (not-empty (explainers expgraph vertex1))
                                  #(hyps-conflict? expgraph %1 %2)
                                  [] ;; explains -- filled in later
                                  (format "%s=%s" vertex1 val1)
                                  (format "%s=%s" vertex1 val1)
                                  {:vertex vertex1 :value val1})})
                 explanatory))
        hyps-explains
        (reduce (fn [m h]
             (let [{:keys [vertex value vertex2 value2 hyp]} h
                   obs (get observed-hyps [vertex2 value2])
                   explains (map :hyp (or (get m [vertex2 value2]) []))
                   obs-expl (if obs (conj explains obs) explains)
                   hyp2 (assoc hyp :explains
                               (filter #(not= (:contents hyp) %)
                                  (map :contents obs-expl)))]
               (if (empty? obs-expl) m
                   (-> m (update-in [[vertex value]] conj (assoc h :hyp hyp2))
                      (assoc [vertex value vertex2 value2] (assoc h :hyp hyp2))))))
           {} (get-hyps hyps-no-explains))]
    (filter identity (map :hyp (get-hyps hyps-explains)))))

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

