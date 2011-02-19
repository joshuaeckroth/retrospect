(ns samre.problems.tracking.evaluate
  (:use [samre.epistemicstates :only (current-ep-state)])
  (:use [samre.workspaces :only [lookup-hyps]])
  (:use [samre.confidences])
  (:require [clojure.set :as set]))

(defn assoc-es-ls
  [ep-state truedata]
  (let [paths (:paths (:problem-data ep-state))]
    (loop [elmap {}
           time 0]
      (if (>= time (:time ep-state)) elmap
          (let [grid (nth truedata time)
                es (filter identity grid)
                ;; does the path explain the entity?
                match? (fn [p e] (some #(and (= (:x (meta %)) (:x (meta e)))
                                             (= (:y (meta %)) (:y (meta e)))
                                             (= (:time (meta %)) (:time (meta e))))
                                       (flatten p)))
                ;; find the label associated with an entity's position/time;
                ;; note that there is only zero or one such label
                find-fn (fn [e] (first (filter (fn [l] (match? (l paths) e)) (keys paths))))
                ;; add to the labels associated with an entity, if there are any such labels
                assoc-fn (fn [elm e] (assoc elm e (if-let [l (find-fn e)]
                                                    (conj (elm e) l) (elm e))))
                ;; add in all the new/updated label associations
                elmap-new (reduce assoc-fn elmap es)]
            (recur elmap-new (inc time)))))))

(defn assoc-es-twl
  "Replace label repeats with counts of the label repeats for each entity."
  [elmap twl e]
  (let [labels (set (elmap e))
        label-counts (map count (for [l labels] (filter #(= l %) (elmap e))))]
    (assoc twl e (if (empty? label-counts) [0] label-counts))))

(defn evaluate
  [ep-state sensors truedata params]
  (let [elmap (assoc-es-ls ep-state truedata)
        twl (reduce (partial assoc-es-twl elmap) {} (keys elmap))]
    {:PercentEventsCorrect 0.0
     :MeanTimeWithLabel (double (/ (reduce + 0 (flatten (vals twl))) (count (keys twl))))
     :MaxTimeWithLabel (double (apply max (flatten (vals twl))))
     :MinTimeWithLabel (double (apply min (flatten (vals twl))))
     :MeanCountAlternatives 0
     :MeanLabelCounts (double (/ (reduce + 0 (map #(count (set (elmap %))) (keys elmap)))
                                 (count (keys elmap))))
     :AvgWalk 0
     :PlausibilityAccuracy 0
     :SensorOverlap 0
     :EntityDensity 0}))

