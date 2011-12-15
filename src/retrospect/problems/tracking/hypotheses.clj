(ns retrospect.problems.tracking.hypotheses
  (:require [clojure.set :as set])
  (:use [clojure.contrib.seq :only [find-first]])
  (:use [retrospect.epistemicstates :only [add-hyp add-more-hyp add-fact]])
  (:use [retrospect.workspaces :only [new-hyp]])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.colors])
  (:use [retrospect.problems.tracking.movements :only [dist dets-match?]])
  (:use [retrospect.problems.tracking.pathsgraph :only
         [paths-graph-paths build-paths-graph path-str]])
  (:use [retrospect.state]))

(def compute 0)
(def memory 0)

(defn make-sensor-hyps
  [sensor {:keys [x y color time] :as det}]
  (let [desc (format (str "Sensor detection by %s - color: %s, x: %d, y: %d, time: %d")
                     (:id sensor) (color-str color) x y time)]
    [(new-hyp "SensFrom" :sensor-from nil 1.0 nil [] [] desc {:sensor sensor :det det})
     (new-hyp "SensTo" :sensor-to nil 1.0 nil [] [] desc {:sensor sensor :det det})]))

(defn process-sensors
  "For each time step between the last time we processed sensor data
   and the current time, look at the sensor detections at that time
   step, create hypotheses out of them, add those hypotheses as
   'facts,' and record them as 'uncovered.'"
  [ep-state sensors time-now]
  (let [pdata (:problem-data ep-state)
        left-off (:left-off pdata)
        det-hyps (mapcat (fn [s]
                           (map (fn [dets] (make-sensor-hyps s dets))
                                (mapcat (fn [t] (sensed-at s t))
                                        (range left-off (inc time-now)))))
                         sensors)
        from-hyps (filter #(< left-off (:time (:det (:data %)))) (map first det-hyps))
        to-hyps (filter #(> time-now (:time (:det (:data %)))) (map second det-hyps))
        ;; filter out old uncovered hyps, so we don't have an explosion of path hyps
        filter-old (fn [hyps] (filter #(>= (:time (:det (:data %)))
                                           (dec left-off))
                                      hyps))
        uncovered-from (set (filter-old (concat (:uncovered-from pdata) from-hyps)))
        uncovered-to (set (filter-old (concat (:uncovered-to pdata) to-hyps)))
        pdata-new (-> pdata
                      (assoc :uncovered-from uncovered-from)
                      (assoc :uncovered-to uncovered-to)
                      (assoc :left-off time-now))
        ep-new (assoc ep-state :problem-data pdata-new)]
    (reduce (fn [ep hyp] (add-fact ep hyp)) ep-new
            (concat (:uncovered-from pdata-new) (:uncovered-to pdata-new)))))

(defn score-movement
  "Returns nil if not matched or not in range."
  [{x1 :x y1 :y t1 :time c1 :color :as det}
   {x2 :x y2 :y t2 :time c2 :color :as det2}
   walk-dist]
  (var-set (var compute) (inc compute))
  (when (and (or (= (inc t1) t2) (= (inc t2) t1))
             (match-color? c1 c2))
    (let [d (dist x1 y1 x2 y2) 
          dist-count (walk-dist d)]
      (if dist-count (double (/ dist-count (:walk-count (meta walk-dist))))))))

(defn make-movement-hyps
  [uncovered-from uncovered-to walk-dist]
  (let [unc-from-by-time (group-by (comp :time :det :data) uncovered-from)
        unc-to-by-time (group-by (comp :time :det :data) uncovered-to)
        ;; pair uncovered dets together, where each pair has a det
        ;; from time t first, and a det from time t+1 second
        unc-pairs (apply concat
                         (for [t (sort (keys unc-to-by-time))]
                           (mapcat (fn [det-hyp]
                                     (map (fn [det2-hyp] [det-hyp det2-hyp])
                                          (sort-by :id (get unc-from-by-time (inc t)))))
                                   (sort-by :id (get unc-to-by-time t)))))
        unc-pairs-scored (map (fn [[det-hyp det2-hyp]]
                                (let [det (:det (:data det-hyp))
                                      det2 (:det (:data det2-hyp))]
                                  {:det-hyp det-hyp :det2-hyp det2-hyp
                                   :det det :det2 det2
                                   :score (score-movement det det2 walk-dist)}))
                              unc-pairs)]
    (for [{:keys [det det-hyp det2 det2-hyp score]} unc-pairs-scored :when score]
      (new-hyp "Mov" :movement nil score
               :and [det-hyp det2-hyp] [det-hyp det2-hyp] (path-str [det det2])
               {:det det :det2 det2
                :movement {:x (:x det2) :y (:y det2) :time (:time det2)
                           :ox (:x det) :oy (:y det) :ot (:time det)
                           :color (cond (not= gray (:color det)) (:color det)
                                        (not= gray (:color det2)) (:color det2)
                                        :else gray)}}))))

(defn avg
  [vals]
  (double (/ (reduce + 0.0 vals) (count vals))))

(defn make-path-hyp
  [bias movs]
  (let [det-seq (sort-by :time (set (mapcat (fn [hyp] [(:det (:data hyp))
                                                       (:det2 (:data hyp))])
                                            movs)))]
    (new-hyp "Path" :path nil
             (avg (map :apriori movs)) :and movs movs
             (format "%s (%s)" (path-str det-seq) (name bias))
             {:movements movs :bias bias})))

(defn make-location-hyp
  "All paths should have the same start and end point and bias."
  [entity prior-loc-hyp bias bias-hyp paths]
  (let [{:keys [x y time color]} (:det2 (:data (last (:movements (:data (first paths))))))]
    (new-hyp "Loc" :location
             [:location entity] ;; conflict with entity/location-hyps
             (apply max (map :apriori paths)) :or paths
             ;; depends
             (concat paths (if prior-loc-hyp [prior-loc-hyp] [])
                     (if bias-hyp [bias-hyp] []))
             (format "Entity %s is at %d,%d at time %d (%s)" entity x y time bias)
             {:entity entity :bias bias :paths paths :color color
              :loc {:x x :y y :time time}})))

(defn extract-path-dets
  [path]
  (concat [(:det (:data (first (:movements (:data path)))))]
          (map (comp :det2 :data) (:movements (:data path)))))

(defn make-location-hyps
  [entities entity-biases path-hyps]
  (mapcat (fn [e] (let [loc (get entities e)
                        matching-starts (filter
                                         #(dets-match? loc (first (extract-path-dets %)))
                                         path-hyps)
                        path-starts-ends (group-by #(last (extract-path-dets %))
                                                   matching-starts)
                        path-biases (map #(group-by (comp :bias :data) %)
                                         (vals path-starts-ends))]
                    (mapcat
                     (fn [pb]
                       (map (fn [b]
                              (make-location-hyp e (:loc-hyp (get entities e))
                                                 (:bias b) (:bias-hyp b)
                                                 (get pb (:bias b))))
                            (filter #(not-empty (get pb (:bias %)))
                                    (if-let [bias-map (get entity-biases e)]
                                      [bias-map] (map (fn [b] {:bias b}) (keys pb))))))
                     path-biases)))
          (keys entities)))

(defn make-bias-hyps
  [loc-hyps]
  (let [unique-last-three-xys
        (fn [loc] (map (fn [path] (set (take-last 3 (extract-path-dets path))))
                       (:paths (:data loc))))
        long-enough-locs (filter (fn [h] (some #(= 3 (count %))
                                               (unique-last-three-xys h))) loc-hyps)
        locs (group-by (comp :entity :data) long-enough-locs)
        biases (mapcat (fn [ls] (vals (group-by (comp :bias :data) ls)))
                       (vals locs))]
    (map (fn [ls]
           (let [entity (:entity (:data (first ls)))
                 bias (:bias (:data (first ls)))]
             (new-hyp "Bias" :bias
                      [:bias entity] ;; conflict with entity/bias-hyps
                      (apply max (map :apriori ls)) :or ls ls
                      (format "Entity %s has bias %s" entity bias)
                      {:entity entity :bias bias :locs ls})))
         biases)))

(defn hypothesize
  "Process sensor reports, then make hypotheses for all possible
  movements, paths, and locations, and add them to the epistemic
  state."
  [ep-state sensors time-now]
  (binding [compute 0 memory 0]
    (let [ep-sensors (process-sensors ep-state sensors time-now)
          pdata (:problem-data ep-sensors)
          {:keys [entities entity-biases accepted unaccepted
                  uncovered-from uncovered-to walk-dist]} pdata
          mov-hyps (make-movement-hyps uncovered-from uncovered-to walk-dist)
          pg (build-paths-graph
              (set/union (set mov-hyps)
                         (set (filter #(= :movement (:type %))
                                      (concat accepted unaccepted)))) entities)
          ep-pg (assoc-in ep-sensors [:problem-data :paths-graph] pg)
          paths (paths-graph-paths pg entities entity-biases)
          path-hyps (filter (fn [h] (some #(not (accepted %)) (:movements (:data h))))
                            (apply concat (for [bias (keys paths)]
                                            (map #(make-path-hyp bias %)
                                                 (filter not-empty (get paths bias))))))
          loc-hyps (make-location-hyps entities entity-biases path-hyps)
          valid-path-hyps (set (mapcat (comp :paths :data) loc-hyps))
          valid-mov-hyps (set (mapcat (comp :movements :data) valid-path-hyps))
          bias-hyps (make-bias-hyps
                     (filter #(nil? (get entity-biases (:entity (:data %))))
                             loc-hyps))]
      [(reduce (fn [ep hyp] (add-hyp ep hyp))
               ep-pg (filter
                      (fn [h] (and (not-any? #(= (:id %) (:id h)) accepted)
                                   (some (fn [e] (not-any? #(= (:id %) (:id e)) accepted))
                                         (:explains h))))
                      (concat valid-mov-hyps valid-path-hyps loc-hyps bias-hyps)))
       {:compute compute :memory memory}])))

(defn no-explainer-hyps
  "Return hypotheses that \"should have\" helped produce explainers
   for the hyps (sensor hyps) given to this function."
  [hyps pdata]
  (let [entities (:entities pdata)
        walk-dist (:walk-dist pdata)]
    (mapcat
     (fn [h]
       (let [det (:det (:data h))
             ;; set color to gray so that any location in range matches
             det2-fn (fn [e es] (let [h (:loc-hyp (get es e))]
                                  (assoc (:loc (:data h)) :color gray)))
             es (filter (fn [e]
                          (binding [compute 0 memory 0]
                            (score-movement det (det2-fn e entities) walk-dist)))
                        (keys entities))]
         (map (fn [e] (:loc-hyp (get entities e))) es)))
     (filter #(or (= :sensor-from (:type %)) (= :sensor-to (:type %))) hyps))))

(defn commit-decision
  [pdata accepted rejected unaccepted time-now]
  (let [entities (reduce (fn [es loc-hyp]
                           (update-in es [(:entity (:data loc-hyp))]
                                      merge (:loc (:data loc-hyp)) {:loc-hyp loc-hyp}))
                         (:entities pdata) (filter #(= :location (:type %))
                                                   accepted))
        entity-biases (reduce (fn [eb bias-hyp]
                                (assoc eb (:entity (:data bias-hyp))
                                       {:bias (:bias (:data bias-hyp))
                                        :bias-hyp bias-hyp}))
                              (:entity-biases pdata) (filter #(= :bias (:type %))
                                                             accepted))
        bel-movs (map (comp :movement :data) (filter #(= :movement (:type %)) accepted))
        dis-movs (map (comp :movement :data) (filter #(= :movement (:type %)) rejected))
        explained-det-hyps (mapcat :explains (filter #(= :movement (:type %)) accepted))
        covered-from (set (filter #(= :sensor-from (:type %)) explained-det-hyps))
        covered-to (set (filter #(= :sensor-to (:type %)) explained-det-hyps))]
    (-> pdata (assoc :entities entities)
        (assoc :entity-biases entity-biases)
        (update-in [:accepted] set/union accepted)
        (update-in [:unaccepted] set/union unaccepted)
        (update-in [:believed-movements] concat bel-movs)
        (update-in [:disbelieved-movements] concat dis-movs)
        (update-in [:uncovered-from] set/difference covered-from)
        (update-in [:uncovered-to] set/difference covered-to))))
