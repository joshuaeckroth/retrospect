(ns retrospect.reason.abdexp.reason
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [loom.graph])
  (:use [retrospect.reason.abdexp.evaluate :only [evaluate evaluate-comp]])
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [retrospect.random])
  (:require [retrospect.state :as state]))

(defn arbitrary
  [expgraph]
  (let [need-expl (sort (need-explanation expgraph))]
    (if (empty? need-expl) expgraph
        (let [expls (filter #(not (conflicts-any? expgraph %))
                            (mapcat #(sort (explainers expgraph %)) need-expl))
              choice (first (my-shuffle expls))]
          (if (nil? choice) expgraph
              (recur (fill expgraph choice)))))))

(defn compare-expls
  [expgraph v1 v2]
  (let [[c1 c2] (map (fn [v] (count (filter #(filled? expgraph %)
                                            (neighbors expgraph v))))
                     [v1 v2])
        [s1 s2] (map (fn [v] (score expgraph v)) [v1 v2])
        [p1 p2] (map (fn [v] (count (filter #(not= v %) (explainers expgraph v))))
                     [v1 v2])]
    (if (= s1 s2)
      (if (and (= c1 c2) (:PreferAbducibles state/params))
        ;; top-level nodes are better;
        ;; otherwise, more explainers is better
        (cond (= 0 p1) -1
              (= 0 p2) 1
              :else (compare p1 p2))
        ;; higher explains counts are better
        (- (compare c1 c2)))
      ;; higher scores are better
      (- (compare s1 s2)))))

(defn compare-delta
  [expgraph vs1 vs2]
  (let [[[c1 & c1rest] [c2 & c2rest]]
        (map (fn [vs] (map (fn [v] (count (filter #(filled? expgraph %)
                                                  (neighbors expgraph v))))
                           vs))
             [vs1 vs2])]
    ;; prefer essential explainers
    (cond (nil? c1rest) -1
          (nil? c2rest) 1
          :else
          ;; higher deltas are better, so compare (- first second)
          (- (compare (- c1 (first c1rest)) (- c2 (first c2rest)))))))

(defn efli
  [expgraph]
  (loop [expgraph expgraph
         deltas {}]
    (let [need-expl (need-explanation expgraph)
          explainers (map (fn [v] (filter #(not (conflicts-any? expgraph %))
                                          (explainers expgraph v)))
                          need-expl)
          expl-sorted (sort (partial compare-delta expgraph)
                            (map #(sort (partial compare-expls expgraph) %)
                                 (filter not-empty explainers)))
          best (ffirst expl-sorted)
          alt (second (first expl-sorted))
          delta (if alt (- (score expgraph best) (score expgraph alt)) 1.0)]
      (if (or (nil? best)
              (and alt (:Scores state/params)
                   (>= (- (/ (:Threshold state/params) 100) 0.0001) delta)))
        [expgraph deltas]
        (recur (fill expgraph best) (assoc deltas best delta))))))

(defn reason
  [workspace time-prev time-now sensors]
  (let [expgraph (sensed-at (first sensors) time-now)
        eg-arb (arbitrary expgraph)
        [eg-efli deltas] (efli expgraph)]
    {:arb eg-arb :efli eg-efli :deltas deltas}))

(def reason-abdexp
  {:name "AbdExp"
   :reason-fn reason
   :evaluate-fn evaluate
   :evaluate-comp evaluate-comp
   :default-params-fn (constantly {:ResetEachStep [true [true]]
                                   :Threshold [0 (range 0 101 10)]})
   :init-workspace-fn (constantly nil)
   :init-kb-fn (constantly nil)
   :player-fns {:get-tabs-fn (constantly [])
                :update-tabs-fn (constantly nil)}})