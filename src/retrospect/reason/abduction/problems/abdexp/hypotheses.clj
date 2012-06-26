(ns retrospect.reason.abduction.problems.abdexp.hypotheses
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.reason.abduction.workspace :only [new-hyp]])
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [loom.graph :only [transpose add-edges]])
  (:use [loom.alg :only [topsort]])
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
        observations (forced-nodes expgraph)]
    (map (fn [v] (new-hyp "Obs" :observation :observation 1.0 true nil []
                       (str v) (str v) {:vertex v}))
       observations)))

(defn hypothesize
  [sensor-hyps accepted lookup-hyp time-now]
  (let [kb (get-kb accepted lookup-hyp)
        expgraph (get (:expgraphs kb) time-now)]
    (loop [hyps (zipmap (map :vertex sensor-hyps) sensor-hyps)
           vertices (rest (topsort (reduce (fn [g v] (add-edges g [-1 v])) (transpose expgraph)
                                      (map :vertex sensor-hyps))
                                   -1))]
      (if (empty? vertices) (filter #(not= :observation (:type %)) (vals hyps))
          (let [v (first vertices)]
            (if (nil? (get hyps v))
              (recur (assoc hyps v (new-hyp "Expl" :expl :expl 1.0
                                            (not-empty (explainers expgraph v))
                                            #(conflicts? expgraph (:vertex %1) (:vertex %2))
                                            (filter identity (map #(get hyps %) (explains expgraph v)))
                                            (format "%s" v) (format "%s @ %d" v time-now)
                                            {:vertex v}))
                     (rest vertices))
              (recur hyps (rest vertices))))))))
