(ns retrospect.problems.tracking.hypotheses
  (:use [retrospect.epistemicstates :only [add-hyp add-more-hyp add-fact]])
  (:use [retrospect.workspaces :only [new-hyp]])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.colors])
  (:use [retrospect.problems.tracking.movements :only [dist]])
  (:use [retrospect.problems.tracking.pathsgraph :only
         [paths-graph-paths build-paths-graph paths-graph-edge-hyps]])
  (:use [retrospect.state]))

;; The goal of the tracking domain is to identify entities' beginning
;; & end positions. At some time t, the tracker is scored by
;; evaluating how many of the entities are believed to hold their
;; true locations at time t. At time 0, the tracker is told the true
;; locations of all the entities. Then, as time progresses, the
;; tracker tries to "follow" the entities (utilizing its knowledge,
;; which may be incomplete, about possible entity motions). At a later
;; time t, the tracker is evaluated in terms of how many of its
;; believed entity positions are accurate.
;;
;; To do its job, the tracker offers three levels of hypotheses for
;; every collection of sensor data. The first level is movement
;; hypotheses: something moves from position x to position y, across
;; one time click. The second level is entity paths: some entity moved
;; from its old location through one or more movements to another
;; location. The third level is entity locations: after a series of
;; movments, the entity is at some location at some time. The location
;; hypotheses explain one or more path hypotheses. The path hypotheses
;; explain one or more movement hypotheses; the movement hypotheses
;; explain sensor detections. If transitive explanation is employed,
;; then in cases where a location hypothesis can be realized by
;; several incompatible path hypotheses, the location hypothesis can
;; still be accepted (without first disambiguating the path
;; hypotheses). The tracker is only ultimately interested in location
;; hypotheses, and only when it has accepted some location hypotheses
;; will it update its beliefs (with new location beliefs).
;;
;; The tracker reports its beliefs as "Entity 0 is at 5,2 at time 8,"
;; for example, for each entity.

(def compute 0)
(def memory 0)

(defn make-sensor-hyps
  [sensor {:keys [x y color time] :as det}]
  (let [desc (format (str "Sensor detection by %s - color: %s, x: %d, y: %d, time: %d")
                     (:id sensor) (color-str color) x y time)]
    (new-hyp "Sens" :sensor nil 1.0 [] desc {:sensor sensor :det det})))

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
                                        (range (inc left-off) (inc time-now)))))
                         sensors)
        pdata-new (-> pdata (update-in [:uncovered] concat det-hyps)
                      (assoc :left-off time-now))
        ep-new (assoc ep-state :problem-data pdata-new)]
    (reduce (fn [ep hyp] (add-fact ep hyp)) ep-new (:uncovered pdata-new))))

(defn score-movement
  "Returns nil if not matched or not in range."
  [{x1 :x y1 :y t1 :time c1 :color :as det}
   {x2 :x y2 :y t2 :time c2 :color :as det2}
   walk-dist]
  (var-set (var compute) (inc compute))
  (when (and (= (inc t1) t2) (match-color? c1 c2))
    (let [d (dist x1 y1 x2 y2) 
          dist-count (walk-dist d)]
      (if dist-count (double (/ dist-count (:walk-count (meta walk-dist))))))))

(defn path-str
  [dets]
  (let [arrows (fn [s] (apply str (interpose " -> " s)))]
    (arrows (map (fn [det] (format "%d,%d@%d (%s)" (:x det) (:y det) (:time det)
                                   (color-str (:color det))))
                 dets))))

(defn make-movement-hyps
  [uncovered walk-dist]
  (let [unc-by-time (group-by (comp :time :det :data) uncovered)
        ;; pair uncovered dets together, where each pair has a det
        ;; from time t first, and a det from time t+1 second
        unc-pairs (apply concat (for [t (butlast (sort (keys unc-by-time)))]
                                  (mapcat (fn [det-hyp]
                                            (map (fn [det2-hyp] [det-hyp det2-hyp])
                                                 (get unc-by-time (inc t))))
                                          (get unc-by-time t))))
        unc-pairs-scored (filter :score
                                 (map (fn [[det-hyp det2-hyp]]
                                        (let [det (:det (:data det-hyp))
                                              det2 (:det (:data det2-hyp))]
                                          {:det-hyp det-hyp :det2-hyp det2-hyp
                                           :det det :det2 det2
                                           :score (score-movement det det2 walk-dist)}))
                                      unc-pairs))]
    (for [{:keys [det det-hyp det2 det2-hyp score]} unc-pairs-scored]
      (new-hyp "Mov" :movement :shared-explains score
               [det-hyp det2-hyp] (path-str [det det2]) {:det det :det2 det2}))))

(defn make-path-hyp
  [movs]
  (let [det-seq (sort-by :time (set (mapcat (fn [hyp] [(:det (:data hyp))
                                                       (:det2 (:data hyp))])
                                            movs)))]
    (new-hyp "Path" :path :shared-explains
             0.0 movs (path-str det-seq) {:movements movs})))

(defn make-location-hyp
  "All paths should have the same start and end point."
  [entity paths]
  (let [{:keys [x y time]} (:det2 (:data (last (:movements (:data (first paths))))))]
    (new-hyp "Loc" :location :shared-explains
             0.0 paths (format "Entity %s is at %d,%d at time %d" entity x y time)
             {:entity entity :paths paths :loc {:x x :y y :time time}})))

(defn dets-match?
  [det det2]
  (and (= (:x det) (:x det2))
       (= (:y det) (:y det2))
       (= (:time det) (:time det2))
       (match-color? (:color det) (:color det2))))

(defn make-location-hyps
  [entities path-hyps]
  (mapcat (fn [e] (let [loc (assoc (get entities e) :color (:color (meta e)))
                        matching-starts (filter
                                         #(dets-match? loc (:det (:data (first (:movements (:data %))))))
                                         path-hyps)
                        path-groups (group-by (fn [hyp] (:det2 (last (:movements hyp))))
                                              matching-starts)]
                    (map #(make-location-hyp e %) (vals path-groups))))
          (keys entities)))

(defn make-dep-node
  [hyp]
  (let [det (if (:det2 (:data hyp)) (:det2 (:data hyp)) (:det (:data hyp)))]
    {:time (:time det) :str (str det)}))

(defn make-dep-nodes
  [hyp]
  [])

(defn hypothesize
  "Process sensor reports, then make hypotheses for all possible
  movements, paths, and locations, and add them to the epistemic
  state."
  [ep-state sensors time-now]
  (binding [compute 0 memory 0]
    (let [ep-sensors (process-sensors ep-state sensors time-now)
          {:keys [entities uncovered walk-dist]} (:problem-data ep-sensors)
          mov-hyps (make-movement-hyps uncovered walk-dist)
          pg (build-paths-graph mov-hyps entities)
          ep-pg (assoc-in ep-sensors [:problem-data :paths-graph] pg)
          valid-mov-hyps (paths-graph-edge-hyps pg)
          path-hyps (map make-path-hyp (paths-graph-paths pg))
          loc-hyps (make-location-hyps entities path-hyps)]
      [(reduce (fn [ep hyp] (add-hyp ep hyp (make-dep-node hyp)
                                     (make-dep-nodes hyp)))
               ep-pg (concat mov-hyps path-hyps loc-hyps))
       {:compute compute :memory memory}])))

;; TODO update uncovered
;; TODO update :believed-movements, :disbelieved-movements
(defn commit-decision
  [pdata accepted rejected time-now]
  (let [entities (assoc pdata :entities
                        (reduce (fn [es loc-hyp] (assoc es (:entity (:data loc-hyp))
                                                        (:loc (:data loc-hyp))))
                                (:entities pdata) (filter #(= :location (:type %))
                                                          accepted)))]))
