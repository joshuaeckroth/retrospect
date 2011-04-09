(ns retrospect.problems.tracking.evaluate
  (:use [retrospect.epistemicstates :only [current-ep-state]])
  (:use [retrospect.confidences])
  (:use [retrospect.colors])
  (:use [retrospect.workspaces :only [get-hyps hyp-conf]])
  (:use [retrospect.problems.tracking.hypotheses :only
         [paths-to-movements path-to-movements]])
  (:use [retrospect.problems.tracking.truedata :only [get-grid-movements]])
  (:use [retrospect.problems.tracking.grid :only [dist]])
  (:require [clojure.set :as set])
  (:use [clojure.contrib.seq :only [find-first]]))

(defn true-movements
  [truedata maxtime]
  (set (map #(dissoc % :e) (get-grid-movements truedata 0 maxtime))))

(defn percent-events-correct
  [pdata true-moves]
  (if (empty? true-moves) 100.0
      (double (* 100.0 (/ (count (set/intersection
                                  true-moves
                                  (set (paths-to-movements (:paths pdata)))))
                          (count true-moves))))))

(defn assoc-es-ls
  [ep-state truedata]
  (let [paths (:paths (:problem-data ep-state))]
    (loop [elmap {}
           time 0]
      (if (or (>= time (:time ep-state))
              (>= time (count truedata))) elmap
          (let [grid (nth truedata time)
                es (flatten grid)
                ;; does the path explain the entity?
                match? (fn [path e] (some #(and (= (:x %) (:x (meta e)))
                                                (= (:y %) (:y (meta e)))
                                                (= (:time %) (:time (meta e)))
                                                (match-color?
                                                 (:color %) (:color (meta e))))
                                          path))
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

(defn plausibility-accuracy
  [prev-ep true-moves]
  ;; the following requires that confidences are really numeric
  ;; and that neutral is 0
  (let [hyps (filter #(re-find #"^M?TH" (:id %)) (get-hyps (:workspace prev-ep)))
        movements (map (fn [h] {:det (:det (:data h)) :det2 (:det2 (:data h))
                                :conf (hyp-conf (:workspace prev-ep) h)})
                       hyps)
        scores (map (fn [{:keys [det det2 conf]}]
                      (if (some
                           #(and (= (:ot %) (:time det)) (= (:t %) (:time det2))
                                 (= (:ox %) (:x det)) (= (:oy %) (:y det))
                                 (= (:x %) (:x det2)) (= (:y %) (:y det2)))
                           true-moves)
                        conf (- 0 conf)))
                    movements)]
    (if (empty? scores) 0.0 (avg scores))))

(defn evaluate
  [ep-state results prev-ep sensors truedata params]
  (let [maxtime (min (dec (dec (count truedata))) (dec (dec (:time ep-state))))
        ;; map of labels per true entity
        elmap (assoc-es-ls ep-state truedata)
        ;; map of time with label (for each label) per true entity
        twl (reduce (partial assoc-es-twl elmap) {} (keys elmap))
        true-moves (true-movements truedata maxtime)]
    {:PercentEventsCorrect (percent-events-correct (:problem-data ep-state) true-moves)
     :MeanTimeWithLabel (avg (map #(avg (twl %)) (keys twl)))
     :MaxTimeWithLabel (avg (map #(apply max (twl %)) (keys twl)))
     :MinTimeWithLabel (avg (map #(apply min (twl %)) (keys twl)))
     :MeanCountAlternatives (mean-count-alts (:workspace ep-state) :sensor)
     :MeanLabelCounts (double (/ (reduce + 0 (map #(count (set (elmap %))) (keys elmap)))
                                 (count (keys elmap))))
     :DistinctLabels (count (keys (:paths (:problem-data ep-state))))
     ;; average current plausibility accuracy with past
     :PlausibilityAccuracy
     (let [pa (plausibility-accuracy prev-ep true-moves)
           c (count results)]
       (if (= c 0) pa
           (/ (+ (* c (:PlausibilityAccuracy (last results))) pa) (inc c))))
     :SensorOverlap 0
     :EntityDensity 0}))

(defn calc-percent-increase
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
   :IncreasePercentEventsCorrect (calc-percent-increase :PercentEventsCorrect m b)
   
   :MetaMeanTimeWithLabel (:MeanTimeWithLabel m)
   :BaseMeanTimeWithLabel (:MeanTimeWithLabel b)
   :RatioMeanTimeWithLabel (calc-ratio :MeanTimeWithLabel m b)
   :IncreaseMeanTimeWithLabel (calc-percent-increase :MeanTimeWithLabel m b)
   
   :MetaMaxTimeWithLabel (:MaxTimeWithLabel m)
   :BaseMaxTimeWithLabel (:MaxTimeWithLabel b)
   :RatioMaxTimeWithLabel (calc-ratio :MaxTimeWithLabel m b)
   :IncreaseMaxTimeWithLabel (calc-percent-increase :MaxTimeWithLabel m b)

   :MetaMinTimeWithLabel (:MinTimeWithLabel m)
   :BaseMinTimeWithLabel (:MinTimeWithLabel b)
   :RatioMinTimeWithLabel (calc-ratio :MinTimeWithLabel m b)
   :IncreaseMinTimeWithLabel (calc-percent-increase :MinTimeWithLabel m b)

   :MetaMeanLabelCounts (:MeanLabelCounts m)
   :BaseMeanLabelCounts (:MeanLabelCounts b)
   :RatioMeanLabelCounts (calc-ratio :MeanLabelCounts m b)
   :IncreaseMeanLabelCounts (calc-percent-increase :MeanLabelCounts m b)

   :MetaDistinctLabels (:DistinctLabels m)
   :BaseDistinctLabels (:DistinctLabels b)
   :RatioDistinctLabels (calc-ratio :DistinctLabels m b)
   :IncreaseDistinctLabels (calc-percent-increase :DistinctLabels m b)

   :MetaPlausibilityAccuracy (:PlausibilityAccuracy m)
   :BasePlausibilityAccuracy (:PlausibilityAccuracy b)
   :RatioPlausibilityAccuracy (calc-ratio :PlausibilityAccuracy m b)
   :IncreasePlausibilityAccuracy (calc-percent-increase :PlausibilityAccuracy m b)
   
   :NumberEntities (:NumberEntities params)
   :StepsBetween (:StepsBetween params)
   :MaxWalk (:MaxWalk params)
   :ProbNewEntities (:ProbNewEntities params)})
