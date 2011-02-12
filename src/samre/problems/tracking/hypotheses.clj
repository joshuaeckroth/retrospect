(ns samre.problems.tracking.hypotheses
  (:use [samre.sensors :only [sensed-at]])
  (:use [samre.problems.tracking.labels :only [man-dist]]))

(defn sensors-to-spotted
  [sensors time sensors-seen-grid]
  (let [width (:width (meta sensors-seen-grid))
        height (:height (meta sensors-seen-grid))]
    (with-meta
      (for [x (range width) y (range height)]
        (apply concat (map (fn [s] (filter (fn [e] (and (= x (:x (meta e)))
                                                        (= y (:y (meta e)))))
                                           (sensed-at s time)))
                           sensors)))
      {:width width :height height :time time})))

(defn process-sensors
  [ep-state sensors time-now]
  (let [sensors-seen-grid (:sensors-seen-grid (:problem-data ep-state))
        spotted-grid
        (loop [t (:time ep-state)
               sg (:spotted-grid (:problem-data ep-state))]
          (if (> t time-now) sg
              (recur (inc t)
                     (conj sg (sensors-to-spotted sensors t sensors-seen-grid)))))]
    (update-in ep-state [:problem-data] assoc :spotted-grid spotted-grid)))

(defn spotted-in-range
  [label grid maxwalk]
  (let [in-range?
        #(>= maxwalk (man-dist (:x label) (:y label) (:x (meta %)) (:y (meta %))))
        spotted-and-in-range?
        #(and (not-empty %) (in-range? (first %)))]
    (filter spotted-and-in-range? grid)))

(defn label-path
  "The first grid in spotted-grid is the time step that should be
  connected; on the recur, strip off the first grid; if spotted-grid
  has no more grids, we're done."
  [label paths spotted-grid maxwalk]
  (if (empty? spotted-grid) paths
      (let [in-range (spotted-in-range label (first spotted-grid) maxwalk)
            extended-paths (map (fn [p] (map #(conj p %) in-range)) paths)]
        (recur label (apply concat extended-paths) (rest spotted-grid) maxwalk))))

(defn hypothesize
  "TODO: hypothesize new labels for spotted that are not put in any paths."
  [ep-state sensors time-now params]
  (let [labels (:labels (:problem-data ep-state))
        ep (process-sensors ep-state sensors time-now)
        spotted-grid (:spotted-grid (:problem-data ep))
        maxwalk (:MaxWalk params)]
    (map (fn [l] {:label l :path (label-path l [] spotted-grid maxwalk)}) labels)))

(defn accept-decision
  [pdata accepted]
  pdata)

