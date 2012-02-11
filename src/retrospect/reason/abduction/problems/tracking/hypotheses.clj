(ns retrospect.reason.abduction.problems.tracking.hypotheses
  (:require [clojure.string :as str])
  (:use [clojure.contrib.seq :only [find-first]])
  (:use [retrospect.reason.abduction.workspace :only [new-hyp]])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.problems.tracking.colors])
  (:use [retrospect.reason.abduction.problems.tracking.evaluate
         :only [hyps-equal?]])
  (:use [retrospect.problems.tracking.movements
         :only [dist dets-match?]])
  (:use [retrospect.reason.abduction.problems.tracking.pathsgraph
         :only [get-paths build-paths-graph path-str]])
  (:use [retrospect.state]))

(comment
  (defn covered?
    [pdata sensor-hyp from-to]
    (some (fn [h] (and (= (:id (:sensor (:data h))) (:id (:sensor (:data sensor-hyp))))
                       (= (:det (:data h)) (:det (:data sensor-hyp)))
                       (= (:subtype h) (:subtype sensor-hyp))))
          (get pdata (if (= :from from-to) :covered-from :covered-to)))))

(defn make-sensor-hyps
  [sensor {:keys [x y color time] :as det} t time-prev time-now]
  (let [desc (format (str "Sensor detection by %s - color: %s, x: %d, y: %d, time: %d")
                     (:id sensor) (color-str color) x y time)
        from (new-hyp "SensFrom" :sensor :sensor-from nil 1.0 [] [] desc
                      {:sensor sensor :det det})
        to (new-hyp "SensTo" :sensor :sensor-to nil 1.0 [] [] desc
                    {:sensor sensor :det det})]
    (cond (= t time-prev) [to]
          (= t time-now) [from]
          :else [from to])))

(comment
  (defn process-sensors
    "Add to \"uncovered-from/to\" sets any sensor data we don't already
   have in one of those sets or the \"covered-from/to\" sets."
    [ep-state sensors time-now]
    (let [pdata (:problem-data ep-state)
          earliest (first (sort (map (comp :time last) (vals (:entities pdata)))))
          det-hyps (mapcat (fn [s] (map (fn [dets] (make-sensor-hyps s dets))
                                        (mapcat (fn [t] (sensed-at s t))
                                                (range earliest (inc time-now)))))
                           sensors)
          from-hyps (set (filter #(and (not (covered? pdata % :from))
                                       (not= earliest (:time (:det (:data %)))))
                                 (map first det-hyps)))
          to-hyps (set (filter #(and (not (covered? pdata % :to))
                                     (not= time-now (:time (:det (:data %)))))
                               (map second det-hyps)))
          pdata-new (assoc pdata :uncovered-from from-hyps :uncovered-to to-hyps)
          ep-new (assoc ep-state :problem-data pdata-new)]
      (reduce (fn [ep hyp] (add-fact ep hyp)) ep-new (concat from-hyps to-hyps)))))

(defn score-movement
  "Returns nil if not matched or not in range."
  [to from walk-dist]
  (let [{x1 :x y1 :y t1 :time c1 :color :as det} (:det to)
        {x2 :x y2 :y t2 :time c2 :color :as det2} (:det from)]
    (when (and (= (inc t1) t2)
               (match-color? c1 c2))
      (let [d (dist x1 y1 x2 y2) 
            dist-count (get walk-dist d)]
        (if dist-count (double (/ dist-count (:walk-count (meta walk-dist))))
            ;; if we don't have learning, make possible movements worth
            ;; a tiny amount if the model doesn't have a frequency for
            ;; this distance
            (if (<= d (* (Math/sqrt 2) (:MaxWalk params)))
              (double (/ 1 (:walk-count (meta walk-dist))))))))))

(comment
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
        (new-hyp "Mov" :movement :movement nil score
                 :and [det-hyp det2-hyp] [det-hyp det2-hyp]
                 (str (path-str [det det2]) " (dist=" (str (dist (:x det) (:y det)
                                                                 (:x det2) (:y det2))) ")")
                 {:det det :det2 det2
                  :det-hyp det-hyp :det2-hyp det2-hyp
                  :movement {:x (:x det2) :y (:y det2) :time (:time det2)
                             :ox (:x det) :oy (:y det) :ot (:time det)
                             :color (cond (not= gray (:color det)) (:color det)
                                          (not= gray (:color det2)) (:color det2)
                                          :else gray)}})))))

(defn avg
  [vals]
  (double (/ (reduce + 0.0 vals) (count vals))))

(comment
  (defn make-path-hyp
    [bias movs]
    (let [det-seq (sort-by :time (set (mapcat (fn [hyp] [(:det (:data hyp))
                                                         (:det2 (:data hyp))])
                                              movs)))]
      (new-hyp "Path" :path :path nil
               (avg (map :apriori movs)) :and movs movs
               (format "%s (%s)" (path-str det-seq) (name bias))
               {:movements movs :bias bias}))))

(comment
  (defn make-location-hyp
    "All paths should have the same start and end point and bias."
    [entity prior-loc-hyp bias bias-hyp paths]
    (let [{:keys [x y time color]} (:det2 (:data (last (:movements (:data (first paths))))))]
      (new-hyp "Loc" :location :location
               [:location entity] ;; conflict with entity/location-hyps
               (apply max (map :apriori paths)) :or paths
               ;; depends
               (concat paths (if prior-loc-hyp [prior-loc-hyp] [])
                       (if bias-hyp [bias-hyp] []))
               (format "Entity %s is at %d,%d at time %d (%s)" entity x y time bias)
               {:entity entity :bias bias :paths paths :color color
                :loc {:x x :y y :time time}}))))

(comment
  (defn extract-path-dets
    [path]
    (concat [(:det (:data (first (:movements (:data path)))))]
            (map (comp :det2 :data) (:movements (:data path))))))

(comment
  (defn make-location-hyps
    [entities entity-biases path-hyps]
    (mapcat (fn [e] (let [loc (last (get entities e))
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
                                (make-location-hyp e (:loc-hyp (last (get entities e)))
                                                   (:bias b) (:bias-hyp b)
                                                   (get pb (:bias b))))
                              (filter #(not-empty (get pb (:bias %)))
                                      (if-let [bias-map (get entity-biases e)]
                                        [bias-map] (map (fn [b] {:bias b}) (keys pb))))))
                       path-biases)))
            (keys entities))))

(comment
  (defn make-bias-hyps
    [loc-hyps]
    (let [unique-last-three-xys
          (fn [loc] (map (fn [path] (set (take-last 3 (map #(dissoc % :time)
                                                           (extract-path-dets path)))))
                         (:paths (:data loc))))
          long-enough-locs (filter (fn [h] (some #(= 3 (count %))
                                                 (unique-last-three-xys h))) loc-hyps)
          locs (group-by (comp :entity :data) long-enough-locs)
          biases (mapcat (fn [ls] (vals (group-by (comp :bias :data) ls)))
                         (vals locs))]
      (map (fn [ls]
             (let [entity (:entity (:data (first ls)))
                   bias (:bias (:data (first ls)))]
               (new-hyp "Bias" :bias :bias
                        [:bias entity] ;; conflict with entity/bias-hyps
                        (apply max (map :apriori ls)) :or ls ls
                        (format "Entity %s has bias %s" entity bias)
                        {:entity entity :bias bias :locs ls})))
           biases))))


(defn conflicts [h1 h2] nil)

(defn read-walk-dist
  [file]
  (let [lines (str/split-lines (slurp file))
        walk-count (Integer/parseInt (first lines))]
    (with-meta (reduce #(assoc %1 (Double/parseDouble (first %2))
                               (Integer/parseInt (second %2)))
                       {} (map #(str/split % #",") (rest lines)))
               {:walk-count walk-count})))

(defn generate-kb
  []
  [(new-hyp "KB" :kb :kb conflicts 1.0 [] [] ""
            {:walk-dist (read-walk-dist (format "%s/tracking/walks-%d.txt"
                                                @datadir (:MaxWalk params)))})])

(defn get-walk-dist
  [accepted]
  (:walk-dist (find-first #(= :kb (:type %)) accepted)))

(defmulti hypothesize
  (fn [evidence accepted rejected hyps] [(:type evidence) (:subtype evidence)]))

(defmethod hypothesize :default [_ _ _ _] [])

(defn filter-existing
  [hyps hs]
  (filter (fn [h] (not-any? (fn [h2] (hyps-equal? h h2)) hyps)) hs))

(defn new-mov-hyp
  [from to apriori]
  (let [det (:det to)
        det2 (:det from)]
    (new-hyp "Mov" :movement :movement conflicts
             apriori [from to] []
             (str (path-str [(:det from) (:det to)]) " (dist="
                  (str (dist (:x (:det from)) (:y (:det to))
                             (:x (:det from)) (:y (:det to)))) ")")
             {:det det :det2 det2
              :mov {:x (:x det2) :y (:y det2) :time (:time det2)
                    :ox (:x det) :oy (:y det) :ot (:time det)
                    :color (cond (not= gray (:color det)) (:color det)
                                 (not= gray (:color det2)) (:color det2)
                                 :else gray)}})))

(defmethod hypothesize [:sensor :sensor-from]
  [evidence accepted rejected hyps]
  (let [sm (fn [h] (score-movement h evidence (get-walk-dist accepted)))
        nearby (filter second (map (fn [h] [h (sm h)])
                                   (filter #(= :sensor-to (:subtype %)) accepted)))]
    (filter-existing hyps (for [[h apriori] nearby]
                            (new-mov-hyp evidence h apriori)))))

(defmethod hypothesize [:sensor :sensor-to]
  [evidence accepted rejected hyps]
  (let [sm (fn [h] (score-movement evidence h (get-walk-dist accepted)))
        nearby (filter second (map (fn [h] [h (sm h)])
                                   (filter #(= :sensor-from (:subtype %)) accepted)))]
    (filter-existing hyps (for [[h apriori] nearby]
                            (new-mov-hyp h evidence apriori)))))

(defn score-path
  [mov-hyps]
  (avg (map :apriori mov-hyps)))

(defmethod hypothesize [:movement :movement]
  [evidence accepted rejected hyps]
  (let [pg (build-paths-graph (filter #(= :movement (:type %)) accepted)
                              (filter #(= :location (:type %)) accepted))
        paths (get-paths pg evidence)]
    (filter-existing hyps (for [p paths]
                            (new-hyp "Path" :path :path conflicts
                                     (score-path p) p []
                                     (path-str (conj (vec (map :det p)) (:det2 (last p))))
                                     {:movs p})))))

(comment
  (defn hypothesize2
    "Process sensor reports, then make hypotheses for all possible
  movements, paths, and locations, and add them to the epistemic
  state."
    [ep-state sensors time-now]
    (let [ep-sensors (process-sensors ep-state sensors time-now)
          pdata (:problem-data ep-sensors)
          {:keys [entities entity-biases accepted
                  uncovered-from uncovered-to walk-dist]} pdata
          acc (set (filter #(not= :sensor (:type %)) accepted))
          mov-hyps (make-movement-hyps uncovered-from uncovered-to walk-dist)
          pg (build-paths-graph (concat mov-hyps (filter #(= :movement (:type %)) acc))
                                entities)
          ep-pg (assoc-in ep-sensors [:problem-data :paths-graph] pg)
          paths nil
          path-hyps (filter
                     (fn [h] (some #(not (acc %)) (:movements (:data h))))
                     (apply concat
                            (for [bias (keys paths)]
                              (map #(make-path-hyp bias %)
                                   (filter
                                    #(and (not-empty %)
                                          (= time-now (:time (:det2 (:data (last %))))))
                                    (get paths bias))))))
          loc-hyps (make-location-hyps entities entity-biases path-hyps)
          valid-path-hyps (set (mapcat (comp :paths :data) loc-hyps))
          valid-mov-hyps (set (mapcat (comp :movements :data) valid-path-hyps))
          bias-hyps (make-bias-hyps
                     (filter #(nil? (get entity-biases (:entity (:data %))))
                             loc-hyps))]
      [(reduce (fn [ep hyp] (add-hyp ep hyp))
               ep-pg (filter (fn [h] (and
                                      ;; don't add a hyp that has been accepted
                                      (not-any? #(= (:id %) (:id h)) acc)
                                      ;; don't add a hyp that explains only accepted hyps
                                      (some (fn [e] (not-any? #(= (:id %) (:id e)) acc))
                                            (:explains h))))
                             (concat valid-mov-hyps valid-path-hyps loc-hyps bias-hyps)))])))

(comment
  (defn get-more-hyps
    [ep-state]
    (let [ws (:workspace ep-state)
          pdata (:problem-data ep-state)
          es-not-updated (filter (fn [e] (not-any? #(and (= :location (:type %))
                                                         (= e (:entity (:data %))))
                                                   (:accepted ws)))
                                 (keys (:entities pdata)))
          dets-unexplained (filter #(= :sensor-from (:subtype %))
                                   (:unexplained (:final (:log ws))))
          loc-hyps (mapcat
                    (fn [e]
                      (let [last-pos (last (get (:entities pdata) e))]
                        (map (fn [d]
                               (new-hyp "LocRec" :location :location-recovery
                                        [:location-recovery e]
                                        (- 1.0
                                           (/ (dist (:x last-pos) (:y last-pos)
                                                    (:x (:det (:data d)))
                                                    (:y (:det (:data d))))
                                              (* (Math/sqrt 2) (:MaxWalk params)
                                                 (- (:time (:det (:data d)))
                                                    (:time last-pos)))))
                                        :or [d] [] ;; explains & depends
                                        (format "Entity %s is at %d,%d at time %d"
                                                e (:x (:det (:data d)))
                                                (:y (:det (:data d)))
                                                (:time (:det (:data d))))
                                        {:entity e :loc (select-keys (:det (:data d))
                                                                     [:x :y :time])}))
                             (filter #(and (match-color?
                                            (:color (first (get (:entities pdata) e)))
                                            (:color (:det (:data %))))
                                           (> (* (Math/sqrt 2) (:MaxWalk params)
                                                 (- (:time (:det (:data %)))
                                                    (:time last-pos)))
                                              (dist (:x last-pos) (:y last-pos)
                                                    (:x (:det (:data %)))
                                                    (:y (:det (:data %))))))
                                     dets-unexplained))))
                    es-not-updated)]
      (reduce (fn [ep hyp] (add-hyp ep hyp)) ep-state loc-hyps))))

(comment
  (defn no-explainer-hyps
    "Return hypotheses that \"should have\" helped produce explainers
   for the hyps (sensor hyps) given to this function."
    [hyps pdata]
    (let [entities (:entities pdata)
          walk-dist (:walk-dist pdata)]
      (set (mapcat
            (fn [h]
              (let [det (:det (:data h))
                    ;; set color to gray so that any location in range matches
                    det2-fn (fn [e es] (let [h2 (:loc-hyp (last (get es e)))]
                                         (assoc (:loc (:data h2)) :color gray)))
                    es (filter (fn [e]
                                 (if (:loc-hyp (last (get entities e)))
                                   (score-movement det (det2-fn e entities) walk-dist)))
                               (keys entities))]
                (map (fn [e] (:loc-hyp (last (get entities e)))) es)))
            (filter #(or (= :sensor-from (:subtype %)) (= :sensor-to (:subtype %))) hyps))))))

(comment
  (defn commit-decision
    [pdata accepted rejected unaccepted time-now]
    (let [entities (reduce (fn [es loc-hyp]
                             (update-in es [(:entity (:data loc-hyp))]
                                        #(conj % (merge (last %) (:loc (:data loc-hyp))
                                                        {:loc-hyp loc-hyp}))))
                           (:entities pdata) (filter #(= :location (:type %))
                                                     accepted))
          entity-biases (reduce (fn [eb bias-hyp]
                                  (assoc eb (:entity (:data bias-hyp))
                                         {:bias (:bias (:data bias-hyp))
                                          :bias-hyp bias-hyp}))
                                (:entity-biases pdata) (filter #(= :bias (:type %))
                                                               accepted))
          bel-movs (set (map (comp :movement :data) (filter #(= :movement (:type %)) accepted)))
          dis-movs (set (map (comp :movement :data) (filter #(= :movement (:type %)) rejected)))
          explained-det-hyps (mapcat :explains
                                     (filter #(or (= :movement (:type %))
                                                  (= :location-recovery (:subtype %)))
                                             accepted))
          covered-from (set (filter #(= :sensor-from (:subtype %)) explained-det-hyps))
          covered-to (set (filter #(= :sensor-to (:subtype %)) explained-det-hyps))]
      (-> pdata (assoc :entities entities)
          (assoc :entity-biases entity-biases)
          (update-in [:accepted] set/union accepted)
          (update-in [:unaccepted] set/union unaccepted)
          (update-in [:believed-movements] set/union bel-movs)
          (update-in [:disbelieved-movements] set/union dis-movs)
          (update-in [:covered-from] set/union covered-from)
          (update-in [:covered-to] set/union covered-to)))))

(comment
  (defn retract
    [pdata hyp]
    (cond (= :bias (:type hyp))
          (-> pdata
              (update-in [:entity-biases] dissoc (:entity (:data hyp)))
              (update-in [:accepted] disj hyp))
          (= :location (:type hyp))
          (-> pdata
              (update-in [:entities (:entity (:data hyp))]
                         (fn [locs] (vec (take-while #(not= (:loc-hyp %) hyp) locs))))
              (update-in [:accepted] disj hyp))
          (= :path (:type hyp))
          (update-in pdata [:accepted] disj hyp)
          (= :movement (:type hyp))
          (-> pdata
              (update-in [:believed-movements] disj (:movement (:data hyp)))
              (update-in [:covered-from] set/difference #{(:det2-hyp (:data hyp))})
              (update-in [:covered-to] set/difference #{(:det-hyp (:data hyp))})
              (update-in [:accepted] disj hyp))
          (or (= :sensor-from (:subtype hyp)) (= :sensor-to (:subtype hyp)))
          (update-in pdata [:accepted] disj hyp)
          :else pdata)))
