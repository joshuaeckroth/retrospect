(ns retrospect.problems.tracking.evaluate
  (:use [retrospect.epistemicstates :only (current-ep-state)])
  (:use [retrospect.confidences])
  (:use [retrospect.problems.tracking.hypotheses :only [path-to-movements]])
  (:use [retrospect.problems.tracking.truedata :only [get-grid-movements]])
  (:require [clojure.set :as set])
  (:use [clojure.contrib.seq :only [find-first]]))

(defn believed-movements
  [pdata]
  (let [paths (:paths pdata)]
    (flatten (map path-to-movements (vals paths)))))

(defn percent-events-correct
  [truedata pdata maxtime]
  (let [grid-movements (set (map #(dissoc % :e) (get-grid-movements truedata 0 maxtime)))
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
                es (flatten grid)
                ;; does the path explain the entity?
                match? (fn [p e] (some #(and (= (:x (meta %)) (:x (meta e)))
                                             (= (:y (meta %)) (:y (meta e)))
                                             (= (:time (meta %)) (:time (meta e))))
                                       (flatten p)))
                ;; find the label associated with an entity's position/time;
                ;; note that there is only zero or one such label
                find-fn (fn [e] (find-first (fn [l] (match? (l paths) e)) (keys paths)))
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

(defn avg
  [l]
  (double (/ (reduce + l) (count l))))

(defn evaluate
  [ep-state sensors truedata params]
  (let [elmap (assoc-es-ls ep-state truedata)
        twl (reduce (partial assoc-es-twl elmap) {} (keys elmap))]
    {:PercentEventsCorrect (percent-events-correct truedata (:problem-data ep-state)
                                                   (dec (dec (:time ep-state))))
     :MeanTimeWithLabel (avg (map #(avg (twl %)) (keys twl)))
     :MaxTimeWithLabel (avg (map #(apply max (twl %)) (keys twl)))
     :MinTimeWithLabel (avg (map #(apply min (twl %)) (keys twl)))
     :MeanCountAlternatives (mean-count-alts (:workspace ep-state) :sensor)
     :MeanLabelCounts (double (/ (reduce + 0 (map #(count (set (elmap %))) (keys elmap)))
                                 (count (keys elmap))))
     :AvgWalk 0
     :PlausibilityAccuracy 0
     :SensorOverlap 0
     :EntityDensity 0}))

(defn calc-percent-improvement
  [k m b]
  (if (= 0 (k b)) 0.0
      (double (* 100.0 (/ (- (k m) (k b)) (k b))))))

(defn calc-ratio
  [k m b]
  (if (= 0 (k b)) 0.0
      (double (/ (k m) (k b)))))

(defn evaluate-batch
  [params [m b]]
  {:MetaPercentEventsCorrect (:PercentEventsCorrect m)
   :BasePercentEventsCorrect (:PercentEventsCorrect b)
   :RatioPercentEventsCorrect (calc-ratio :PercentEventsCorrect m b)
   :ImprovePercentEventsCorrect (calc-percent-improvement :PercentEventsCorrect m b)
   
   :MetaMeanTimeWithLabel (:MeanTimeWithLabel m)
   :BaseMeanTimeWithLabel (:MeanTimeWithLabel b)
   :RatioMeanTimeWithLabel (calc-ratio :MeanTimeWithLabel m b)
   :ImproveMeanTimeWithLabel (calc-percent-improvement :MeanTimeWithLabel m b)
   
   :MetaMaxTimeWithLabel (:MaxTimeWithLabel m)
   :BaseMaxTimeWithLabel (:MaxTimeWithLabel b)
   :RatioMaxTimeWithLabel (calc-ratio :MaxTimeWithLabel m b)
   :ImproveMaxTimeWithLabel (calc-percent-improvement :MaxTimeWithLabel m b)

   :MetaMinTimeWithLabel (:MinTimeWithLabel m)
   :BaseMinTimeWithLabel (:MinTimeWithLabel b)
   :RatioMinTimeWithLabel (calc-ratio :MinTimeWithLabel m b)
   :ImproveMinTimeWithLabel (calc-percent-improvement :MinTimeWithLabel m b)

   :MetaMeanLabelCounts (:MeanLabelCounts m)
   :BaseMeanLabelCounts (:MeanLabelCounts b)
   :RatioMeanLabelCounts (calc-ratio :MeanLabelCounts m b)
   :ImproveMeanLabelCounts (calc-percent-improvement :MeanLabelCounts m b)
   
   :NumberEntities (:NumberEntities params)
   :MaxWalk (:MaxWalk params)
   :ProbNewEntities (:ProbNewEntities params)})
