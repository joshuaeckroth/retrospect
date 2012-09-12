(ns retrospect.reason.abduction.problems.abdexp.hypotheses
  (:require [clojure.set :as set])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.reason.abduction.workspace :only [new-hyp]])
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
               (assoc hyps v (new-hyp "Obs" :observation :observation (score expgraph v "on") true
                                      #(hyps-conflict? expgraph %1 %2)
                                      (map #(:contents (get hyps %)) (explains expgraph v))
                                      (str v) (str v)
                                      {:vertex v :value value}))
               (rest vertices))))))))

(defn hypothesize
  [unexp-hyps accepted lookup-hyp time-now]
  (let [kb (get-kb accepted lookup-hyp)
        expgraph (:expgraph kb)
        sensor-hyps (filter #(= :observation (:type %)) unexp-hyps)]
    (loop [hyps (zipmap (map :vertex sensor-hyps) (map (fn [hyp] [hyp]) sensor-hyps))
           vertices (filter #(not (observation? %)) (sorted-by-dep expgraph))]
      (if (empty? vertices)
        (filter #(not= :observation (:type %))
           (filter identity (mapcat #(get hyps %) (sorted-by-dep expgraph))))
        (let [v (first vertices)]
          (if (nil? (get hyps v))
            (recur (assoc hyps v
                          (doall (for [value (values expgraph v)]
                                   (new-hyp "Expl" :expl :expl (score expgraph v value)
                                            (not-empty (explainers expgraph v))
                                            #(hyps-conflict? expgraph %1 %2)
                                            ;; use the filter because
                                            ;; some observations that
                                            ;; this hyp explains may not
                                            ;; be observed yet
                                            (set (filter identity (mapcat #(map :contents (get hyps %))
                                                                   (explains expgraph v))))
                                            (format "%s" v) (format "%s @ %d" v time-now)
                                            {:vertex v :value value}))))
                   (rest vertices))
            (recur hyps (rest vertices))))))))
