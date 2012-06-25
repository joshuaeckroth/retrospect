(ns retrospect.reason.abduction.problems.abdexp.hypotheses
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.reason.abduction.workspace :only [new-hyp]])
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [loom.graph :only [transpose add-edges]])
  (:use [loom.alg :only [bf-traverse]])
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
        data (data-nodes expgraph)]
    (map (fn [d] (new-hyp "Data" :data :data 1.0 true nil []
                       (str d) (str d)
                       {:vertex d :time time-now
                        :expgraph expgraph}))
       data)))

(defn hypothesize
  [sensor-hyps accepted lookup-hyp]
  (let [kb (get-kb accepted lookup-hyp)
        time (:time (first sensor-hyps))
        expgraph (get (:expgraphs kb) time)]
    (loop [hyps (zipmap (map :vertex sensor-hyps) sensor-hyps)
           vertices (rest (bf-traverse (reduce (fn [g v] (add-edges g [-1 v])) (transpose expgraph)
                                          (map :vertex sensor-hyps))
                                       -1))]
      (if (empty? vertices) (filter #(not= :data (:type %)) (vals hyps))
          (let [v (first vertices)]
            (if (nil? (get hyps v))
              (recur (assoc hyps v (new-hyp "Expl" :expl :expl 1.0
                                            (not-empty (explainers expgraph v))
                                            #(conflicts? expgraph (:vertex %1) (:vertex %2))
                                            (map #(get hyps %) (explains expgraph v))
                                            (format "%s" v) (format "%s @ %d" v time)
                                            {:vertex v :time time
                                             :expgraph expgraph}))
                     (rest vertices))
              (recur hyps (rest vertices))))))))
