(ns retrospect.reason.abdexp.reason
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [loom.graph])
  (:use [retrospect.reason.abdexp.evaluate :only [evaluate evaluate-comp]])
  (:use [retrospect.problems.abdexp.expgraph]))

(defn arbitrary
  [expgraph]
  (let [need-expl (need-explanation expgraph)]
    (if (empty? need-expl) expgraph
        (let [expls (filter #(not (conflicts-any? expgraph %))
                            (mapcat #(incoming expgraph %) need-expl))]
          (if (empty? expls) expgraph
              (recur (fill expgraph (first (shuffle expls)))))))))

(defn compare-expl-count
  [expgraph v1 v2]
  (let [[c1 c2] (map (fn [v] (count (filter #(filled? expgraph %)
                                            (neighbors expgraph v))))
                     [v1 v2])]
    (- (compare c1 c2))))

(defn compare-delta-expl-count
  [expgraph vs1 vs2]
  (let [[[c1 & c1rest] [c2 & c2rest]]
        (map (fn [vs] (map (fn [v] (count (filter #(filled? expgraph %)
                                                  (neighbors expgraph v))))
                           vs))
             [vs1 vs2])]
    ;; prefer essential explainers
    (cond (nil? c1rest) 1
          (nil? c2rest) -1
          :else
          (- (compare (- (first c1rest) c1) (- (first c2rest) c2))))))

(defn efli
  [expgraph]
  (let [need-expl (need-explanation expgraph)
        explainers (map (fn [v]
                          (sort (partial compare-expl-count expgraph)
                                (filter #(not (conflicts-any? expgraph %))
                                        (incoming expgraph v))))
                        need-expl)
        expl-sorted (map #(sort (partial compare-expl-count expgraph) %)
                         (filter not-empty explainers))
        best (ffirst (sort (partial compare-delta-expl-count expgraph) expl-sorted))]
    (if-not best expgraph
            (recur (fill expgraph best)))))

(defn reason
  [workspace time-prev time-now sensors]
  (let [expgraph (sensed-at (first sensors) time-now)
        eg-arb (arbitrary expgraph)
        eg-efli (efli expgraph)]
    {:arb eg-arb :efli eg-efli}))

(def reason-abdexp
  {:name "AbdExp"
   :reason-fn reason
   :evaluate-fn evaluate
   :evaluate-comp evaluate-comp
   :default-params-fn (constantly {:ResetEachStep [true [true]]})
   :init-workspace-fn (constantly nil)
   :init-kb-fn (constantly nil)
   :player-fns {:get-tabs-fn (constantly [])
                :update-tabs-fn (constantly nil)}})
