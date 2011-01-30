(ns samre.problems.tracking.sensors
  (:require [samre.problems.tracking positions])
  (:require [samre workspaces sensors])
  (:import [samre.problems.tracking.positions Position])
  (:import [samre.sensors Sensor])
  (:import [samre.workspaces Hypothesis])
  (:use [samre.confidences])
  (:use [samre.colors])
  (:use [samre.problems.tracking.entities :only (EntityMethods pos)])
  (:use [samre.problems.tracking.grid :only (entity-at)])
  (:use [samre.problems.tracking.eventlog :only (get-entities)])
  (:use [samre.sensors :only (init-sensor add-sensed)]))

(defn sensor-entity-to-str
  [hyp]
  (format "SensorEntity %s %s (a=%s, c=%s) %s@%d"
          (name (:id hyp)) (color-str (:color (:data hyp)))
          (confidence-str (:apriori hyp))
          (confidence-str (:confidence hyp)) (str (:pos (:data hyp)))
          (:time (:data hyp))))

(defn make-sensorentity-id
  [pos time]
  (keyword (format "SE%d-%d-%d" time (:x pos) (:y pos))))

(defn make-sensorentity-whereto-id
  [pos time]
  (keyword (format "SW%d-%d-%d" time (:x pos) (:y pos))))

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

(defn list-sensors-seen
  [width height sensors]
  (doall (filter identity
                 (for [x (range width) y (range height)]
                   (if (some #(sees % x y) sensors) (Position. x y))))))

(defn list-sensors-unseen
  [width height sensors]
  (let [seen (list-sensors-seen width height sensors)]
    (doall (filter identity
                   (for [x (range width) y (range height)]
                     (let [p (Position. x y)]
                       (if-not (some #(= % p) seen) p)))))))

(defn make-spotted-whereto-hyps
  [spotted time-now]
  (let [mk-hyp (fn [s] (Hypothesis.
                        (make-sensorentity-whereto-id (:pos (:data s)) (:time (:data s)))
                        :spotted-whereto
                        VERY-PLAUSIBLE VERY-PLAUSIBLE
                        [] (constantly []) (constantly [])
                        (fn [h t sb] (> (- t (:time (:data s))) sb))
                        sensor-entity-to-str
                        {:time (:time (:data s)) :pos (:pos (:data s))
                         :color (:color (:data s)) :spotted s}))]
    (map (fn [s] {:spotted s
                  :hyp (if (> time-now (:time (:data s))) (mk-hyp s))})
         spotted)))

(defn find-spotted
  [sensor grid time]
  (doall (map #(Hypothesis. (make-sensorentity-id (pos %) time)
                            :sensor-entity
                            VERY-PLAUSIBLE VERY-PLAUSIBLE
                            [] (constantly []) (constantly [])
                            (fn [h t sb] (> (- t time) sb))
                            sensor-entity-to-str
                            {:time time :pos (pos %)
                             :color
                             (if (:sees-color (:attributes sensor)) (:color %) gray)})
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
  [id left right bottom top sees-color]
  (init-sensor sense {:id id :left left :right right :bottom bottom :top top
                      :sees-color sees-color}))

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

(defn sensor-inside-another?
  [sensor sensors]
  (let [inside? (fn [s] (and
                         (not= (:id (:attributes sensor)) (:id (:attributes s)))
                         (>= (sens-left sensor) (sens-left s))
                         (<= (sens-right sensor) (sens-right s))
                         (>= (sens-bottom sensor) (sens-bottom s))
                         (<= (sens-top sensor) (sens-top s))))]
    (some inside? sensors)))

(defn generate-sensors-sample
  [width height sees-color-prob]
  (doall (for [i (range (rand-int (* width height)))]
           (let [left (rand-int width)
                 right (+ left (rand-int (- width left)))
                 bottom (rand-int height)
                 top (+ bottom (rand-int (- height bottom)))]
             (new-sensor (keyword (format "Sensor%d" (hash (rand))))
                         left right bottom top
                         (> (double (/ sees-color-prob 100)) (rand)))))))

(defn generate-sensors-with-coverage
  [width height coverage sees-color-prob]
  (loop [sensors (generate-sensors-sample width height sees-color-prob)]
    (let [measured (measure-sensor-coverage width height sensors)]
      (if (and (> measured (- coverage 5.0)) (< measured (+ coverage 5.0)))
        (filter #(not (sensor-inside-another? % sensors)) sensors)
        (recur (generate-sensors-sample width height sees-color-prob))))))

(defn generate-sensors
  [params]
  (generate-sensors-with-coverage
    (:GridWidth params) (:GridHeight params)
    (:SensorCoverage params) (:SensorSeesColor params)))

