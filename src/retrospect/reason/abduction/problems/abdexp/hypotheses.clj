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
       (or (and (= (:type hyp1) (:type hyp2))
                (= (:vertex hyp1) (:vertex hyp2))
                (not= (:value hyp1) (:value hyp2)))
           (conflicts? expgraph
                       [(:vertex hyp1) (:value hyp1)]
                       [(:vertex hyp2) (:value hyp2)]))))

(defn make-hyp
  [expgraph hyps v val]
  (new-hyp "Expl" :expl :expl (score expgraph v val)
           (not-empty (explainers expgraph v)) #(hyps-conflict? expgraph %1 %2)
           (map #(:contents (get hyps %))
              (mapcat (fn [v2] (map (fn [val2] [v2 val2]) (values expgraph v2)))
                      (explains expgraph v)))
           (format "%s=%s" v val)
           (format "%s=%s\ninitial score: %.2f" v val (score expgraph v val))
           {:vertex v :value val}))

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
         (reduce (fn [m val] (assoc m [v val] (make-hyp expgraph hyps v val)))
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

(defn update-kb
  [accepted unexplained hypotheses lookup-hyp]
  (map lookup-hyp (get accepted :kb)))

(defn make-sensor-hyps
  "Pick out the hyps that have been observed."
  [sensors time-prev time-now accepted all-hyps lookup-hyp]
  (if (= time-prev time-now) []
      (let [expgraph (:expgraph (get-kb accepted lookup-hyp))
            observed (set (mapcat #(sensed-at (first sensors) %)
                                  (range (inc time-now))))]
        (for [[v val] observed]
          (new-hyp "Obs" :observation :observation
                   1.0 true #(hyps-conflict? expgraph %1 %2)
                   [] (format "Observed %s=%s" v val) (format "Observed %s=%s" v val)
                   {:vertex v :value val})))))

(defn explanatory?-gardenfors-delta
  "Is P(vertex2=val2|vertex1=val1) > P(vertex2=val2) and
   P(vertex1=val1) < 1? If so, vertex1=val1 is explanatory
   (according to Gardenfors)."
  [bn vertex1 val1 vertex2 val2 observed]
  (unobserve-all bn)
  (let [prior (get-posterior-marginal bn vertex1 val2)]
    (and (< prior 1.0)
         (do (observe-seq bn (filter #(not= vertex2 (first %)) observed))
             (let [prior (get-posterior-marginal bn vertex2 val2)]
               (observe bn vertex1 val1)
               (let [posterior (get-posterior-marginal bn vertex2 val2)]
                 (> posterior prior)))))))

(defn explanatory?-conditional
  "Is P(vertex2=val2|vertex1=val1) > 0.5?"
  [bn vertex1 val1 vertex2 val2 observed]
  (unobserve-all bn)
  (observe-seq bn (filter #(not= vertex2 (first %)) observed))
  (observe bn vertex1 val1)
  (let [posterior (get-posterior-marginal bn vertex2 val2)]
    (> posterior 0.5)))

(defn explanatory?
  [bn vertex1 val1 vertex2 val2 observed]
  (cond (= "gardenfors-delta" (:ExplanatoryDef state/params))
        (explanatory?-gardenfors-delta bn vertex1 val1 vertex2 val2 observed)
        (= "conditional" (:ExplanatoryDef state/params))
        (explanatory?-conditional bn vertex1 val1 vertex2 val2 observed)
        :else true))

(defn update-explanatory
  "Returns a hyp map (keyed by [vertex value]) of all hyps in the
   graph with their :explains info updated to respect
   the :ExplanatoryDef parameter and the current accepted hyps, which
   are taken to be observations."
  [expgraph bn acc-hyps expl-hyps]
  (let [observed (map (fn [h] [(:vertex h) (:value h)]) acc-hyps)
        find-explains (fn [h] (let [vs-vals
                                   (mapcat (fn [v2] (map (fn [val2] [v2 val2])
                                                      (values expgraph v2)))
                                           (explains expgraph (:vertex h)))]
                               (filter (fn [[v2 val2]]
                                    (explanatory? bn (:vertex h) (:value h)
                                                  v2 val2 observed))
                                  vs-vals)))]
    ;; update hyps in hyp map with what they explain
    (reduce (fn [m h] (assoc-in m [[(:vertex h) (:value h)] :explains]
                          (map #(:contents (get m %)) (find-explains h))))
       ;; initialize hyp map
       (reduce (fn [m h] (assoc m [(:vertex h) (:value h)] h))
          {} expl-hyps)
       expl-hyps)))

(defn hypothesize
  [unexp-hyps accepted all-hyps lookup-hyp time-now]
  (let [kb (get-kb accepted lookup-hyp)
        bn (:bayesnet kb)
        expgraph (:expgraph kb)
        obs-hyps (filter #(= :observation (:type %)) unexp-hyps)
        acc-hyps (map lookup-hyp (concat (:expl accepted) (:observation accepted)))
        expl-hyps (map lookup-hyp (:expl all-hyps))
        hyp-map (update-explanatory expgraph bn acc-hyps expl-hyps)]
    ;; now update scores
    (unobserve-all bn)
    (observe-seq bn (map (fn [h] [(:vertex h) (:value h)]) acc-hyps))
    (let [hyp-map-scores (reduce (fn [m [v val]]
                              (assoc-in m [[v val] :apriori]
                                        (get-posterior-marginal bn v val)))
                            hyp-map (keys hyp-map))]
      (vals (reduce (fn [m obs-hyp]
                 ;; update existing expl hyps that are equivalent to obs hyps, so
                 ;; that each expl hyp explains its corresponding obs hyp
                 (update-in m [[(:vertex obs-hyp) (:value obs-hyp)] :explains]
                            conj (:contents obs-hyp)))
               hyp-map-scores obs-hyps)))))

