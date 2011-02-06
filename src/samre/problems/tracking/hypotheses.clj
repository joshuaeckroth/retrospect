(ns samre.problems.tracking.hypotheses)

(defn sensors-to-spotted
  [sensors time sensors-seen-grid]
  (let [width (:width (meta sensors-seen-grid))
        height (:height (meta sensors-seen-grid))]
    (with-meta
      (for [x (range width) y (range height)]
        (apply concat (map (fn [s] (filter (fn [e] (and (= x (:x (meta e)))
                                                        (= y (:y (meta e)))
                                                        (= time (:time (meta e)))))
                                           (sensed-from s time)))
                           sensors)))
      {:width width :height height})))

(defn process
  [ep-state sensors]
  (update-in ep-state [:spotted-grid :problem-data]
             conj (sensors-to-spotted sensors (:time ep-state)
                                      (:sensors-seen-grid (:problem-data ep-state)))))

(defn commit
  [ep-state time])

(defn str-fn
  [hyp]
  (format "TrackingHyp %s (a=%s, c=%s)\nEntity: %s\nEvent: %s\nExplains: %s"
          (name (:id hyp)) (confidence-str (:apriori hyp))
          (confidence-str (:confidence hyp))
          (str (:entity (:data hyp)))
          (str (:event (:data hyp)))
          (apply str (interpose "," (sort (map name (:explains hyp)))))))

(defn ancient-fn
  [old-time hyp new-time sb]
  (> (- new-time old-time) sb))

(def implausible-fn (constantly []))

(defn impossible-fn
  "Two tracking hypotheses are completely incompatible ('impossible'
  together) when they explain at least one common sensor detection
  or start from the same position."
  [hyp hyps]
  (let [explains (set (:explains hyp))
        entity (:entity (:data hyp))
        event (:event (:data hyp))]
    (filter (fn [h]
              (let [event-h (:event (:data h))]
                (and
                 (not= hyp h)
                 (= (:type h) :tracking)
                 (or
                  ;; same oldpos/oldtime (ignoring EventNew)?
                  (and (not (nil? (:oldpos event)))
                       (not (nil? (:oldpos event-h)))
                       (= (:x (:oldpos event)) (:x (:oldpos event-h)))
                       (= (:y (:oldpos event)) (:y (:oldpos event-h)))
                       (= (:oldtime event) (:oldtime event-h)))
                  ;; have at least one common explainer
                  (not-empty (set/intersection (set (:explains h)) explains))))))
            hyps)))

(defn in-range?
  ([pos1 pos2 n params]
     (<= (manhattan-distance pos1 pos2) (* n (:MaxWalk params))))
  ([pos1 pos2 params]
     (<= (manhattan-distance pos1 pos2) (:MaxWalk params))))

(defn score-event-new
  [event params]
  (if (= (:time event) 0) VERY-PLAUSIBLE (prob-apriori (:ProbNewEntities params))))

(defn score-event-appear
  [event params]
  NEUTRAL)

(defn score-event-frozen
  [event params]
  (prob-neg-apriori (:ProbMovement params)))

(defn score-event-move
  [event params]
  (let [dist (manhattan-distance (:oldpos event) (:pos event))]
    (cond (<= dist (math/ceil (/ (:MaxWalk params) 4)))
          PLAUSIBLE
          (<= dist (math/ceil (/ (:MaxWalk params) 3)))
          NEUTRAL
          (<= dist (math/ceil (/ (:MaxWalk params) 2)))
          IMPLAUSIBLE
          :else ;; equiv to <= dist (:MaxWalk params)
          VERY-IMPLAUSIBLE)))

(defn score-event-disappear
  [event params]
  NEUTRAL)

(defn score-event-disreappear
  [event params]
  NEUTRAL)

(defn score-event
  [event params]
  (let [t (type event)]
    (cond (= t samre.problems.tracking.events.EventNew)
          (score-event-new event params)
          (= t samre.problems.tracking.events.EventAppear)
          (score-event-appear event params)
          (= t samre.problems.tracking.events.EventDisappear)
          (score-event-disappear event params)
          (= t samre.problems.tracking.events.EventFrozen)
          (score-event-frozen event params)
          (= t samre.problems.tracking.events.EventMove)
          (score-event-move event params)
          (= t samre.problems.tracking.events.EventDisReappear)
          (score-event-disreappear event params))))

(defn make-hyp
  [h entity event color explains params]
  (let [apriori (score-event event params)]
    (Hypothesis. (keyword (format "TH%d" (hash [(rand) h])))
                 :tracking
                 apriori apriori
                 (map :id explains)
                 implausible-fn
                 impossible-fn
                 (partial ancient-fn (:time event))
                 str-fn
                 {:entity entity :event event :color color})))

(defn make-entity-whereto-hyps
  [entities time-prev]
  (let [mk-hyp (fn [e] (Hypothesis. (keyword (format "TW%d" (hash [(rand) e])))
                                    :entity-whereto VERY-PLAUSIBLE VERY-PLAUSIBLE
                                    [] (constantly []) (constantly [])
                                    (partial ancient-fn (inc (:time (last (:snapshots e)))))
                                    str-fn {:entity e}))]
    (map (fn [e] {:entity e :hyp (mk-hyp e)})
         (filter #(= time-prev (inc (:time (last (:snapshots %))))) entities))))

(defn generate-initial-movements
  "Generate initial movements from existing entities (s = start, e = end)."
  [es-with-hyps nodes params]
  (for [s es-with-hyps e (filter (fn [n] (and
                                          (match-color? (:entity s) n)
                                          (= (inc (:time (last (:snapshots (:entity s)))))
                                             (:time n))
                                          (not= (pos (:entity s)) (:pos n))
                                          (in-range? (pos (:entity s)) (:pos n) params)))
                                 nodes)]
    (let [event (EventMove. (:time e) (dec (:time e)) (:pos e) (pos (:entity s)))]
      (make-hyp [(:entity s) (:time e) (:pos e)]
                (:entity s) event (:color e) [(:hyp s) (:spotted e)] params))))

(defn generate-movements
  "Generate movements between sensor entities."
  [nodes params]
  (for [s nodes e (filter (fn [n] (and
                                   (match-color? s n)
                                   (= (inc (:time s)) (:time n))
                                   (not= (:pos s) (:pos n))
                                   (in-range? (:pos s) (:pos n) params))) nodes)]
    (let [event (EventMove. (:time e) (dec (:time e)) (:pos e) (:pos s))]
      (make-hyp [s (:time s) (:pos s) (:time e) (:pos e)]
                nil event (:color e) [(:spotted e) (:hyp s)] params))))

(defn generate-new
  "Generate new entity events."
  [nodes time params]
  (if (not (or (= time 0) (< 0 (:ProbNewEntities params)))) []
      (for [e nodes]
        (let [event (EventNew. (:time e) (:pos e))]
          (make-hyp [(:time e) (:pos e) "new"] (new-entity (:time e) (:pos e) (:color e))
                    event (:color e) [(:spotted e)] params)))))

(defn generate-appearances
  "Generate entity appearances."
  [nodes params pdata]
  (for [e (filter #(some (fn [p] (in-range? (:pos %) p params))
                         (:sensors-unseen pdata)) nodes)]
    (let [event (EventAppear. (:time e) (:pos e))]
      (make-hyp [(:time e) (:pos e) "appear"] (new-entity (:time e) (:pos e) (:color e))
                event (:color e) [(:spotted e)] params))))

(defn generate-frozen
  "Generate frozen entity events which occupy positions of existing entities."
  [es-with-hyps nodes params]
  (for [s es-with-hyps e (filter (fn [n] (and
                                          (match-color? (:entity s) n)
                                          (= (inc (:time (last (:snapshots (:entity s)))))
                                             (:time n))
                                          (= (pos (:entity s)) (:pos n)))) nodes)]
    (let [event (EventFrozen. (:time e) (dec (:time e)) (:pos e) (:pos e))]
      (make-hyp [(:entity s) (:time e) (:pos e)]
                (:entity s) event (:color e) [(:hyp s) (:spotted e)] params))))

(defn generate-detected-frozen
  "Generate frozen entity events among sensor detections."
  [nodes params]
  (for [s nodes e (filter (fn [n] (and
                                   (match-color? s n)
                                   (= (inc (:time s)) (:time n))
                                   (= (:pos s) (:pos n)))) nodes)]
    (let [event (EventFrozen. (:time e) (dec (:time e)) (:pos e) (:pos e))]
      (make-hyp [(:time s) (:pos s) (:time e) (:pos e)]
                nil event (:color e) [(:spotted e) (:hyp s)] params))))

(defn generate-disappearances
  [es-with-hyps nodes params pdata]
  (let [ss (filter #(some (fn [p] (in-range? (pos (:entity %)) p params))
                          (:sensors-unseen pdata))
                   es-with-hyps)]
    (for [s ss]
      (let [last-time (:time (last (:snapshots (:entity s))))
            event (EventDisappear. (inc last-time) last-time (pos (:entity s)))]
        (make-hyp [(:entity s) last-time (pos (:entity s)) "disappear"]
                  (:entity s) event (:color s) [(:hyp s)] params)))))

(defn generate-disreappearances
  "Generate disappearance (and reappearances) when an old detection or entity
   had enough time (2 time steps) and was within range of an unseen area to
   have possibly disappeared and reappeared."
  [es-with-hyps nodes params pdata]
  (let [ss (filter #(some (fn [p] (in-range? (pos (:entity %)) p params))
                          (:sensors-unseen pdata))
                   es-with-hyps)
        es (filter #(some (fn [p] (in-range? (:pos %) p params)) (:sensors-unseen pdata))
                   nodes)
        close-entity (fn [s e] (let [dt (- (:time e) (:time (last (:snapshots s))))]
                                 (and (match-color? s e) (<= 2 dt)
                                      (in-range? (pos s) (:pos e) dt params))))
        close-spotted (fn [s e] (let [dt (- (:time e) (:time s))]
                                  (and (match-color? s e) (<= 2 dt)
                                       (in-range? (:pos s) (:pos e) dt params))))]
    (concat
     ;; s = start, e = end
     ;; from entities to unseen to detected
     (for [s ss e (filter #(close-entity (:entity s) %) es)]
       (let [event (EventDisReappear. (:time e) (:time (last (:snapshots (:entity s))))
                                    (:pos e) (pos (:entity s)))]
         (make-hyp [(:entity s) (:time e) (:pos e)]
                   (:entity s) event (:color e) [(:hyp s) (:spotted e)] params)))
     ;; from detected to unseen to detected
     (for [s es e (filter #(close-spotted s %) es)]
       (let [event (EventDisReappear. (:time e) (:time s) (:pos e) (:pos s))]
         (make-hyp [(:time s) (:pos s) (:time e) (:pos e)]
                   nil event (:color e) [(:spotted e) (:hyp s)] params))))))

(defn generate-all-links
  "For each existing entity and each sensor detection, generate the
  following events: a movement event to every sensor detection that is
  one time step ahead; a frozen event at the existing location; and a
  new event at the existing location."
  [es-with-hyps spotted params time pdata]
  (let [nodes (map (fn [{s :spotted h :hyp}]
                     {:time (:time (:data s)) :pos (:pos (:data s))
                      :color (:color (:data s)) :spotted s :hyp h}) spotted)]
    (concat
     (generate-initial-movements es-with-hyps nodes params)
     (generate-movements nodes params)
     (generate-new nodes time params)
     (generate-appearances nodes params pdata)
     (generate-disappearances es-with-hyps nodes params pdata)
     (generate-frozen es-with-hyps nodes params)
     (generate-detected-frozen nodes params)
     (generate-disreappearances es-with-hyps nodes params pdata))))

(defn get-hyps
  [ep-state time-prev time-now sensors params]
  (let [spotted-by-sensors (apply concat (map #(sensed-from % time-prev) sensors))
        unique-spotted (vals (apply merge (map (fn [s] {(:id s) s}) spotted-by-sensors)))
        spotted-with-hyps (make-spotted-whereto-hyps unique-spotted time-now)
        entities (get-entities (:eventlog (:problem-data ep-state)))
        ;; hypothesize and state as fact the sensor detections and whereto hyps
        ep (reduce (fn [ep {s :spotted h :hyp}]
                     (let [ep2 (-> ep (add-hyp s) (force-acceptance s))]
                       (if h (-> ep2 (add-hyp h) (force-acceptance h)) ep2)))
                   ep-state spotted-with-hyps)
        es-with-hyps (make-entity-whereto-hyps entities time-prev)
        ep-entity-hyps (reduce (fn [ep eh] (-> ep (add-hyp (:hyp eh))
                                               (force-acceptance (:hyp eh))))
                               ep es-with-hyps)
        ;; hypothesize the hyps
        hyps (generate-all-links
              es-with-hyps spotted-with-hyps params time-now (:problem-data ep-entity-hyps))
        ep-hyps (reduce add-hyp ep-entity-hyps hyps)
        ep-time (assoc ep-hyps :time time-now)]
    ep-time))

(defn get-more-hyps
  [ep-state time-prev time-now sensors params lazy]
  (get-hyps ep-state time-prev time-now sensors params))

(defn update-eventlog
  "Given an entity and events (in hyps), update the event log."
  [entity hyps pdata]
  (assoc pdata :eventlog
         (reduce (fn [el hyp]
                   (let [e (:entity (:data hyp))
                         ev (:event (:data hyp))
                         color (:color (:data hyp))]
                     (cond
                      (= (type ev) samre.problems.tracking.events.EventNew)
                      (-> el
                          (add-event ev)
                          (add-entity e))
                      (= (type ev) samre.problems.tracking.events.EventAppear)
                      (-> el
                          (add-event ev)
                          (add-entity e))
                      (= (type ev) samre.problems.tracking.events.EventDisappear)
                      el ;; do nothing
                      (= (type ev) samre.problems.tracking.events.EventFrozen)
                      (-> el
                          (update-entity (:time ev) entity (:pos ev)))
                      (= (type ev) samre.problems.tracking.events.EventMove)
                      (-> el
                          (add-event ev)
                          (update-entity (:time ev) entity (:pos ev) color))
                      (= (type ev) samre.problems.tracking.events.EventDisReappear)
                      (-> el
                          (add-event ev)
                          (update-entity (:time ev) entity (:pos ev) color)))))
                 (:eventlog pdata) (sort-by (comp :time :event :data) hyps))))

(defn connect-hyps
  [hyps entity]
  (loop [path (vec (filter #(= (:id (:entity (:data %))) (:id entity)) hyps))]
    (if (empty? path) []
        (let [event (:event (:data (last path)))
              end-time (:time event)
              end-pos (:pos event)
              same-start (filter #(and (= (:oldpos (:event (:data %))) end-pos)
                                       (= (:oldtime (:event (:data %))) end-time))
                                 hyps)
              next (first (sort-by (comp :time :event :data) same-start))]
          (if (nil? next) path
              (recur (conj path next)))))))

(defn accept-decision
  [pdata hyps]
  (let [new-entities (map (comp :entity :data)
                          (filter #(or (= (type (:event (:data %)))
                                          samre.problems.tracking.events.EventNew)
                                       (= (type (:event (:data %)))
                                          samre.problems.tracking.events.EventAppear))
                                  hyps))]
    (reduce (fn [pd entity] (update-eventlog entity (connect-hyps hyps entity) pd))
            pdata (concat new-entities (get-entities (:eventlog pdata))))))
