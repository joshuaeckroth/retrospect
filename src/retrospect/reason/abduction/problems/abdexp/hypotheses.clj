(ns retrospect.reason.abduction.problems.abdexp.hypotheses
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.reason.abduction.workspace :only [new-hyp]])
  (:use [retrospect.problems.abdexp.expgraph])
  (:require [retrospect.state :as state]))

(defn generate-kb
  [training]
  [(new-hyp "KB" :kb :kb 1.0 false nil [] "" "" {:expgraphs (:test training)})])

(defn get-kb
  [accepted lookup-hyp]
  (lookup-hyp (first (get accepted :kb))))

(defn update-kb
  [accepted unexplained hypotheses lookup-hyp]
  (map lookup-hyp (get accepted :kb)))

(defn make-sensor-hyps
  [sensor time-prev time-now accepted lookup-hyp]
  (let [expgraph (sensed-at sensor time-now)
        prev-hyps (map lookup-hyp (:all accepted))
        observed (set (forced-nodes expgraph))]
    (loop [hyps (zipmap (map :vertex prev-hyps) prev-hyps)
           vertices (filter observed (sorted-by-dep expgraph))]
      (if (empty? vertices)
        (filter #(= :observation (:type %)) (map #(get hyps %) (sorted-by-dep expgraph)))
        (let [v (first vertices)]
          (recur
           (assoc hyps v (new-hyp "Obs" :observation :observation 1.0 true
                                  #(conflicts? expgraph (:vertex %1) (:vertex %2))
                                  (map #(:contents (get hyps %)) (explains expgraph v))
                                  (str v) (str v)
                                  {:vertex v}))
           (rest vertices)))))))

(defn hypothesize
  [sensor-hyps forced-hyps accepted lookup-hyp time-now]
  (let [kb (get-kb accepted lookup-hyp)
        expgraph (get (:expgraphs kb) time-now)]
    (loop [hyps (zipmap (map :vertex forced-hyps) forced-hyps)
           vertices (sorted-by-dep expgraph)]
      (if (empty? vertices)
        (filter #(not= :observation (:type %)) (map #(get hyps %) (sorted-by-dep expgraph)))
        (let [v (first vertices)]
          (if (nil? (get hyps v))
            (recur (assoc hyps v (new-hyp "Expl" :expl :expl 1.0
                                          (not-empty (explainers expgraph v))
                                          #(conflicts? expgraph (:vertex %1) (:vertex %2))
                                          (map #(:contents (get hyps %)) (explains expgraph v))
                                          (format "%s" v) (format "%s @ %d" v time-now)
                                          {:vertex v}))
                   (rest vertices))
            (recur hyps (rest vertices))))))))
