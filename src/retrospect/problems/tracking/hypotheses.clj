(ns retrospect.problems.tracking.hypotheses
  (:use [retrospect.epistemicstates :only [add-hyp add-fact]])
  (:use [retrospect.workspaces :only [new-hyp]])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.colors])
  (:use [retrospect.confidences])
  (:use [retrospect.problems.tracking.grid :only [grid-at dist]])
  (:use [clojure.contrib.seq :only [find-first]])
  (:require [clojure.contrib.math :as math])
  (:require [clojure.set :as set :only [intersection difference]]))

(defn make-sensor-hyps
  [sensor e]
  (let [desc (format (str "Sensor detection by %s - color: %s, "
                          "x: %d, y: %d, time: %d")
                     (:id sensor) (color-str (:color (meta e)))
                     (:x (meta e)) (:y (meta e)) (:time (meta e)))]
    (with-meta e (merge (meta e)
                        (zipmap [:hyp-from :hyp-to]
                                (map #(new-hyp % :sensor nil NEUTRAL desc
                                               {:sensor sensor :entity e})
                                     ;; sensor hyp 'from', sensor hyp 'to'
                                     ["SHF" "SHT"]))))))

(defn sensors-to-spotted
  [sensors time sensors-seen-grid]
  (let [width (:width (meta sensors-seen-grid))
        height (:height (meta sensors-seen-grid))]
    (with-meta
      (doall (for [y (range height) x (range width)] ;; need height followed by width
               (mapcat (fn [s]
                         (map #(make-sensor-hyps s %)
                              (filter (fn [e] (and (= x (:x (meta e)))
                                                   (= y (:y (meta e)))))
                                      (sensed-at s time))))
                       sensors)))
      {:width width :height height :time time})))

(defn process-sensors
  "For each time step between the last time we processed sensor data and the current time,
   look at the sensor detections at that time step, incorporate them into our running
   estimate of the grid, add those detections as 'facts,' and record those detections
   as 'uncovered.'"
  [ep-state sensors time-now]
  (let [sensors-seen-grid (:sensors-seen-grid (:problem-data ep-state))]
    (loop [t (inc (apply max -1 (map (comp :time meta)
                                     (flatten (:spotted-grid (:problem-data ep-state))))))
           sg (:spotted-grid (:problem-data ep-state))
           uncovered (:uncovered (:problem-data ep-state))
           ep ep-state]
      (if (> t time-now)
        (update-in ep [:problem-data] assoc :spotted-grid sg :uncovered uncovered)
        (let [spotted (sensors-to-spotted sensors t sensors-seen-grid)
              flat-spotted (flatten spotted)]
          (recur (inc t) (conj sg spotted)
                 (reduce (fn [unc sp] (conj unc (select-keys (meta sp) [:x :y :time :color])))
                         uncovered flat-spotted)
                 (reduce #(add-fact %1 %2 [])
                         ep (mapcat (fn [s] [(:hyp-from (meta s)) (:hyp-to (meta s))])
                                    flat-spotted))))))))

(defn score-distance
  "Returns nil if movement is impossible."
  [x1 y1 x2 y2 maxwalk]
  (let [d (dist x1 y1 x2 y2)
        mw (* (math/sqrt 2.1) maxwalk)]
    (cond
     (<= d (/ mw 4.0)) VERY-PLAUSIBLE
     (<= d (/ mw 3.0)) PLAUSIBLE
     (<= d (/ mw 2.0)) NEUTRAL
     (<= d (/ (* mw 2.0) 3.0)) IMPLAUSIBLE
     (<= d mw) VERY-IMPLAUSIBLE)))

(defn score-movement
  "Returns nil if movement is impossible."
  [{x1 :x y1 :y t1 :time c1 :color} {x2 :x y2 :y t2 :time c2 :color} params]
  (score-distance x1 y1 x2 y2 (:MaxWalk params)))

(defn matched-and-in-range?
  "Returns nil if not matched or not in range."
  [{x1 :x y1 :y t1 :time c1 :color :as det} {x2 :x y2 :y t2 :time c2 :color :as det2} params]
  (when (and (= (inc t1) t2) (match-color? c1 c2))
    (when-let [score (score-movement det det2 params)]
      score)))

(defn make-movement-hyps
  [det uncovered spotted-grid params]
  (let [find-spotted (fn [d] (filter #(= (:color d) (:color (meta %)))
                                     (grid-at (nth spotted-grid (:time d))
                                              (:x d) (:y d))))
        desc-fn (fn [det det2 explains]
                  (format "%s->%s\nExplains: %s"
                          det det2 (apply str (interpose "," (map :id explains)))))]
    (filter identity
            (for [det2 uncovered]
              (when-let [score (matched-and-in-range? det det2 params)]
                ;; it is important that the hyp explains where det2 is 'from' and
                ;; where det went 'to'
                (let [explains (concat (map (comp :hyp-from meta) (find-spotted det2))
                                       (map (comp :hyp-to meta) (find-spotted det)))]
                  [(new-hyp "TH" :tracking nil score (desc-fn det det2 explains)
                            {:det det :det2 det2})
                   explains]))))))

(defn hypothesize
  "Process sensor reports, then make hypotheses for all possible movements,
   and add them to the epistemic state."
  [ep-state sensors time-now params]
  (let [ep (process-sensors ep-state sensors time-now)
        sg (:spotted-grid (:problem-data ep))
        uncovered (:uncovered (:problem-data ep))]
    (loop [hyps []
           unc uncovered]
      (if (empty? unc)
        ;; ran out of uncovered detections; so add all the hyps
        (reduce (fn [ep [hyp explains]] (add-hyp ep hyp explains)) ep hyps)
        ;; take the first uncovered detection, and make movement hyps out of it
        (let [mov-hyps (make-movement-hyps (first unc) uncovered sg params)]
          (recur (concat hyps mov-hyps) (rest unc)))))))

(defn find-color
  "Find first non-gray color, if there is one."
  [paths]
  (let [c (find-first #(not= % gray) (map (comp :color meta) (flatten paths)))]
    (or c gray)))

(defn paths-str
  [paths]
  "")

(defn path-to-movements
  [paths]
  [])

(defn commit-decision
  [pdata accepted rejected shared-explains unexplained time-now]
  pdata)

