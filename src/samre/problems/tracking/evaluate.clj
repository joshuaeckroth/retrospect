(ns samre.problems.tracking.evaluate
  (:use [samre.epistemicstates :only (current-ep-state)])
  (:use [samre.workspaces :only [lookup-hyps]])
  (:use [samre.confidences])
  (:use [samre.problems.tracking.hypotheses :only [path-to-movements]])
  (:require [clojure.set :as set]))

(defn find-grid-movements
  [truedata maxtime]
  (if (< maxtime 0) []
      (flatten
       (for [time (range (inc maxtime))]
         (let [grid-before (nth truedata time)
               grid-after (nth truedata (inc time))]
           (map (fn [e] (let [e2 (first (filter #(= e %) grid-after))]
                          {:ox (:x (meta e)) :oy (:y (meta e)) :ot (:time (meta e))
                           :x (:x (meta e2)) :y (:y (meta e2)) :t (:time (meta e2))}))
                (filter identity grid-before)))))))

(defn believed-movements
  [pdata]
  (let [paths (:paths pdata)]
    (flatten (map path-to-movements (vals paths)))))

(defn percent-events-correct
  [truedata pdata maxtime]
  (let [grid-movements (set (find-grid-movements truedata maxtime))
        bel-movements (set (believed-movements pdata))]
    (if (empty? grid-movements) 100.0
        (double (* 100.0 (/ (count (set/intersection grid-movements bel-movements))
                            (count grid-movements)))))))

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

(defn mean-count-alts
  [workspace type]
  (let [hyp-map-empty (reduce (fn [m hyp] (assoc m (:id hyp) 0))
                              {} (filter #(= type (:type %)) (vals (:hyps workspace))))
        hyp-map (reduce (fn [m hyp-id] (if (nil? (hyp-id m)) m
                                            (update-in m [hyp-id] inc)))
                        hyp-map-empty (flatten (map :explains (vals (:hyps workspace)))))]
    (if (empty? hyp-map) 0.0
        (double (/ (reduce + 0 (vals hyp-map)) (count hyp-map))))))

(defn evaluate
  [ep-state sensors truedata params]
  (let [elmap (assoc-es-ls ep-state truedata)
        twl (reduce (partial assoc-es-twl elmap) {} (keys elmap))]
    {:PercentEventsCorrect (percent-events-correct truedata (:problem-data ep-state)
                                                   (dec (dec (:time ep-state))))
     :MeanTimeWithLabel (double (/ (reduce + 0 (flatten (vals twl))) (count (keys twl))))
     :MaxTimeWithLabel (double (apply max (flatten (vals twl))))
     :MinTimeWithLabel (double (apply min (flatten (vals twl))))
     :MeanCountAlternatives (mean-count-alts (:workspace ep-state) :sensor)
     :MeanLabelCounts (double (/ (reduce + 0 (map #(count (set (elmap %))) (keys elmap)))
                                 (count (keys elmap))))
     :AvgWalk 0
     :PlausibilityAccuracy 0
     :SensorOverlap 0
     :EntityDensity 0}))

