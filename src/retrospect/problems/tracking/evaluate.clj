(ns retrospect.problems.tracking.evaluate
  (:use [retrospect.epistemicstates :only [current-ep-state]])
  (:use [retrospect.confidences])
  (:use [retrospect.colors])
  (:use [retrospect.workspaces :only [get-hyps hyp-conf get-doubt]])
  (:use [retrospect.problems.tracking.hypotheses :only
         [paths-to-movements path-to-movements]])
  (:use [retrospect.problems.tracking.truedata :only
         [true-movements]])
  (:use [retrospect.problems.tracking.grid :only [dist grid-entities]])
  (:require [clojure.set :as set])
  (:use [clojure.contrib.seq :only [find-first]]))

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
                es (grid-entities grid)
                ;; does the path explain the entity?
                match? (fn [path e]
                         (let [{:keys [x y time color]} (meta e)]
                           (some #(and (= (:x %) x) (= (:y %) y)
                                       (= (:time %) time) (match-color? (:color %) color))
                                 path)))
                ;; find the label(s) associated with an entity's position/time
                find-fn (fn [e] (filter (fn [l] (match? (l paths) e)) (keys paths)))
                ;; add to the labels associated with an entity, if there are any such labels
                assoc-fn (fn [elm e] (update-in elm [e] concat (find-fn e)))
                ;; add in all the new/updated label associations
                elmap-new (reduce assoc-fn elmap es)]
            (recur elmap-new (inc time)))))))

(defn assoc-es-twl
  "Replace label repeats with counts of the label repeats for each entity."
  [elmap twl e]
  (assoc twl e (frequencies (elmap e))))

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
  (if (empty? l) 0
      (double (/ (reduce + l) (count l)))))

;; TODO: determine if this matches new continuous plausibilities
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

(defn plausibility-events
  [prev-ep true-moves plausibility]
  (let [hyps (filter #(re-find #"^M?TH" (:id %)) (get-hyps (:workspace prev-ep)))
        movements (map (fn [h] {:det (:det (:data h)) :det2 (:det2 (:data h))})
                       (filter #(= plausibility (hyp-conf (:workspace prev-ep) %)) hyps))
        scores (map (fn [{:keys [det det2]}]
                      (if (some
                           #(and (= (:ot %) (:time det)) (= (:t %) (:time det2))
                                 (= (:ox %) (:x det)) (= (:oy %) (:y det))
                                 (= (:x %) (:x det2)) (= (:y %) (:y det2)))
                           true-moves)
                        1 0))
                    movements)]
    (if (empty? scores) 0.0 (avg scores))))

(defn avg-with-prior
  [results key val]
  (let [c (count results)]
    (cond (= c 0) val
          (= 0.0 val) (key (last results))
          :else (double (/ (+ (* c (key (last results))) val) (inc c))))))

(defn evaluate
  [ep-state results prev-ep sensors truedata params]
  (let [maxtime (min (dec (dec (count truedata))) (dec (dec (:time ep-state))))
        ;; map of labels per true entity
        elmap (assoc-es-ls ep-state truedata)
        ;; map of time with label (for each label) per true entity
        twl (reduce (partial assoc-es-twl elmap) {} (keys elmap))
        true-moves (true-movements truedata maxtime)
        pec (percent-events-correct (:problem-data ep-state) true-moves)
        log (:log (:workspace prev-ep))]
    {:PEC pec
     :CountRemoved
     (if (< 0 (count results))
       (+ (:CountRemoved (last results))
          (:count-removed (:problem-data ep-state)))
       (:count-removed (:problem-data ep-state)))
     :CountRemovedPercent
     (let [tracking-hyps
           (count (set/difference (set (:added log))
                                  (set (:forced log))))]
       (avg-with-prior results :CountRemovedPercent
         (if (= 0 tracking-hyps) 0.0
             (double (/ (:count-removed (:problem-data ep-state))
                        tracking-hyps)))))
     :PlausibilityWorkspaceAccuracy
     (if (= "A" (:id prev-ep)) 0
         (avg-with-prior results :PlausibilityWorkspaceAccuracy
           (- pec (get-doubt (:workspace prev-ep)))))
     :MTL (avg (map #(avg (vals (twl %))) (keys twl)))
     :MeanCountAlternatives (mean-count-alts (:workspace ep-state) :sensor)
     :MLC (double (/ (reduce + 0 (map #(count (set (elmap %)))
                                                  (keys elmap)))
                                 (count (keys elmap))))
     :DistinctLabels (count (keys (:paths (:problem-data ep-state))))
     ;; average current plausibility accuracy with past
     :PlausibilityAccuracy (avg-with-prior results :PlausibilityAccuracy
                             (plausibility-accuracy prev-ep true-moves))
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

(defn evaluate-meta
  [ep-state meta-ep-state results truedata params]
  (let [maxtime (min (dec (dec (count truedata))) (dec (dec (:time ep-state))))
        true-moves (true-movements truedata maxtime)
        ;; map of labels per true entity
        elmap (assoc-es-ls ep-state truedata)
        elmap-meta (assoc-es-ls meta-ep-state truedata)
        ;; map of time with label (for each label) per true entity
        twl (reduce (partial assoc-es-twl elmap) {} (keys elmap))
        twl-meta (reduce (partial assoc-es-twl elmap-meta) {} (keys elmap-meta))
        pec (percent-events-correct
             (:problem-data ep-state) true-moves)
        pec-meta (percent-events-correct
                  (:problem-data meta-ep-state) true-moves)]
    {:AvgMetaDiffPEC (avg-with-prior results :AvgMetaDiffPEC (- pec-meta pec))
     :AvgMetaDiffMTL
     (avg-with-prior results :AvgMetaDiffMTL
       (- (avg (map #(avg (vals (twl-meta %))) (keys twl-meta)))
          (avg (map #(avg (vals (twl %))) (keys twl)))))
     :AvgMetaDiffMLC
     (avg-with-prior results :AvgMetaDiffMLC
       (- (double (/ (reduce + 0 (map #(count (set (elmap-meta %)))
                                      (keys elmap-meta)))
                     (count (keys elmap-meta))))
          (double (/ (reduce + 0 (map #(count (set (elmap %)))
                                      (keys elmap)))
                     (count (keys elmap))))))}))

(defn evaluate-comparative
  [params m b]
  {:MetaPEC (:PEC m)
   :BasePEC (:PEC b)
   :RatioPEC (calc-ratio :PEC m b)
   :IncreasePEC (calc-percent-increase :PEC m b)

   :MetaMTL (:MTL m)
   :BaseMTL (:MTL b)
   :RatioMTL (calc-ratio :MTL m b)
   :IncreaseMTL (calc-percent-increase :MTL m b)
   
   :MetaMLC (:MLC m)
   :BaseMLC (:MLC b)
   :RatioMLC (calc-ratio :MLC m b)
   :IncreaseMLC (calc-percent-increase :MLC m b)

   :MetaDistinctLabels (:DistinctLabels m)
   :BaseDistinctLabels (:DistinctLabels b)
   :RatioDistinctLabels (calc-ratio :DistinctLabels m b)
   :IncreaseDistinctLabels (calc-percent-increase :DistinctLabels m b)

   :MetaPlausibilityAccuracy (:PlausibilityAccuracy m)
   :BasePlausibilityAccuracy (:PlausibilityAccuracy b)
   :RatioPlausibilityAccuracy (calc-ratio :PlausibilityAccuracy m b)
   :IncreasePlausibilityAccuracy (calc-percent-increase :PlausibilityAccuracy m b)

   :MetaPlausibilityWorkspaceAccuracy (:PlausibilityWorkspaceAccuracy m)
   :BasePlausibilityWorkspaceAccuracy (:PlausibilityWorkspaceAccuracy b)
   :RatioPlausibilityWorkspaceAccuracy (calc-ratio :PlausibilityWorkspaceAccuracy m b)
   :IncreasePlausibilityWorkspaceAccuracy
   (calc-percent-increase :PlausibilityWorkspaceAccuracy m b)
   
   :NumberEntities (:NumberEntities params)
   :StepsBetween (:StepsBetween params)
   :MaxWalk (:MaxWalk params)
   :SensorSeesColor (:SensorSeesColor params)
   :GridWidth (:GridWidth params)
   :GridHeight (:GridHeight params)
   :ProbNewEntities (:ProbNewEntities params)})
