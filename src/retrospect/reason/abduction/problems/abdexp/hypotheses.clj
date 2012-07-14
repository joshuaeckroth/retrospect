(ns retrospect.reason.abduction.problems.abdexp.hypotheses
  (:require [clojure.set :as set])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.reason.abduction.workspace :only [new-hyp]])
  (:use [retrospect.problems.abdexp.expgraph])
  (:require [retrospect.state :as state]))

(defn generate-kb
  [training]
  [(new-hyp "KB" :kb :kb 1.0 false nil [] "" "" {:expgraphs (:test training)})])

(defn get-kb
  [accepted lookup-hyp]
  (first (filter :expgraphs (map lookup-hyp (get accepted :kb)))))

(defn update-kb
  [accepted unexplained hypotheses lookup-hyp]
  (map lookup-hyp (get accepted :kb)))

(defn make-sensor-hyps
  [sensors time-prev time-now accepted lookup-hyp]
  (let [kb (get-kb accepted lookup-hyp)
        expgraph (get (:expgraphs kb) time-now)
        prev-hyps (map lookup-hyp (:all accepted))
        observed (reduce set/union (map #(sensed-at (first sensors) %)
                                 (range (inc time-now))))]
    (loop [hyps (zipmap (map :vertex prev-hyps) prev-hyps)
           vertices observed]
      (if (empty? vertices)
        (filter #(= :observation (:type %)) (vals hyps))
        (let [v (first vertices)]
          (recur
           (assoc hyps v (new-hyp "Obs" :observation :observation (score expgraph v) true
                                  #(or (conflicts? expgraph (:vertex %1) (:vertex %2))
                                       (and (= :ignore (:subtype %1)) (= (:vertex %1) (:vertex %2)))
                                       (and (= :ignore (:subtype %2)) (= (:vertex %1) (:vertex %2))))
                                  (map #(:contents (get hyps %)) (explains expgraph v))
                                  (str v) (str v)
                                  {:vertex v}))
           (rest vertices)))))))

(defn hypothesize
  [unexp-hyps accepted lookup-hyp time-now]
  (let [kb (get-kb accepted lookup-hyp)
        expgraph (get (:expgraphs kb) time-now)
        sensor-hyps (filter #(= :observation (:type %)) unexp-hyps)]
    (loop [hyps (zipmap (map :vertex sensor-hyps) sensor-hyps)
           vertices (filter #(not (observation? %)) (sorted-by-dep expgraph))]
      (if (empty? vertices)
        (filter #(not= :observation (:type %))
           (filter identity (map #(get hyps %) (sorted-by-dep expgraph))))
        (let [v (first vertices)]
          (if (nil? (get hyps v))
            (recur (assoc hyps v (new-hyp "Expl" :expl :expl (score expgraph v)
                                          (not-empty (explainers expgraph v))
                                          #(conflicts? expgraph (:vertex %1) (:vertex %2))
                                          (map #(:contents (get hyps %)) (explains expgraph v))
                                          (format "%s" v) (format "%s @ %d" v time-now)
                                          {:vertex v}))
                   (rest vertices))
            (recur hyps (rest vertices))))))))
