(ns samre.problems.tracking.sensors
  (:require [samre.problems.tracking positions])
  (:require [samre workspaces sensors])
  (:import [samre.problems.tracking.positions Position])
  (:import [samre.sensors Sensor])
  (:import [samre.workspaces Hypothesis])
  (:use [samre.confidences])
  (:use [samre.problems.tracking.entities :only (EntityMethods pos)])
  (:use [samre.problems.tracking.grid :only (entity-at)])
  (:use [samre.problems.tracking.eventlog :only (get-entities)])
  (:use [samre.sensors :only (init-sensor add-sensed)]))

(defn sensor-entity-to-str
  [hyp]
  (format "SensorEntity %s (a=%s, c=%s) %s@%d"
          (name (:id hyp)) (confidence-str (:apriori hyp))
          (confidence-str (:confidence hyp)) (str (:pos (:data hyp)))
          (:time (:data hyp))))

(defn make-sensorentity-id
  [pos time]
  (keyword (format "SE%d-%d-%d" time (:x pos) (:y pos))))

(defn sens-left
  [sensor]
  (:left (:attributes sensor)))

(defn sens-right
  [sensor]
  (:right (:attributes sensor)))

(defn sens-top
  [sensor]
  (:top (:attributes sensor)))

(defn sens-bottom
  [sensor]
  (:bottom (:attributes sensor)))

(defn sees
  [sensor x y]
  (and (>= x (sens-left sensor)) (<= x (sens-right sensor))
       (>= y (sens-bottom sensor)) (<= y (sens-top sensor))))

(defn find-spotted
  [sensor grid time]
  (doall (map #(Hypothesis. (make-sensorentity-id (pos %) time)
                            :sensor-entity
                            VERY-PLAUSIBLE VERY-PLAUSIBLE
                            [] (constantly []) (constantly [])
                            identity
                            (fn [h t] (> (- t time) 3))
                            sensor-entity-to-str
                            {:time time :pos (pos %)})
              (filter #(not (nil? %))
                      (doall (for [x (range (sens-left sensor) (inc (sens-right sensor)))
                                   y (range (sens-bottom sensor) (inc (sens-top sensor)))]
                               (entity-at grid (Position. x y))))))))

(defn sense
  [sensor {grid :grid} time]
  (let [spotted (find-spotted sensor grid time)]
    (add-sensed sensor time spotted)))

(defn new-sensor
  "Generate a new sensor with provided values and an empty 'spotted' vector."
  [id left right bottom top]
  (init-sensor sense {:id id :left left :right right :bottom bottom :top top}))

(defn measure-sensor-coverage
  [width height sensors]
  (let [markers (for [x (range width) y (range height)]
		  (if (some #(sees % x y) sensors) 1 0))
	covered (reduce + markers)]
    (double (* 100 (/ covered (* width height))))))

(defn measure-sensor-overlap
  [width height sensors]
  (let [count-xy
	(doall (for [x (range width) y (range height)]
                 (count (filter identity (map (fn [s] (sees s x y)) sensors)))))]
    (double (/ (reduce + count-xy) (* width height)))))

(defn generate-sensors-sample
  [width height]
  (doall (for [i (range (rand-int (* width height)))]
           (let [left (rand-int width)
                 right (+ left (rand-int (- width left)))
                 bottom (rand-int height)
                 top (+ bottom (rand-int (- height bottom)))]
             (new-sensor "sensorid" left right bottom top)))))

(defn generate-sensors-with-coverage
  [width height coverage]
  (loop [sensors (generate-sensors-sample width height)]
    (let [measured (measure-sensor-coverage width height sensors)]
      (if (and (> measured (- coverage 5.0)) (< measured (+ coverage 5.0))) sensors
	  (recur (generate-sensors-sample width height))))))

(defn generate-sensors
  [params]
  (generate-sensors-with-coverage
    (:GridWidth params) (:GridHeight params) (:SensorCoverage params)))

