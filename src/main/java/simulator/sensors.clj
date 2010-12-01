(ns simulator.sensors)

(defrecord Sensor
    [sense-fn
     sensed
     attributes])

(defn init-sensor
  [sense-fn attributes]
  (Sensor. sense-fn {} attributes))

(defn sensed-at
  [sensor time]
  (get (:sensed sensor) time))

(defn add-sensed
  [sensor time data]
  (update-in sensor [:sensed] assoc time data))

(defn update-sensors
  [sensors moment time]
  (doall (map (fn [s] ((:sense-fn s) s moment time)) sensors)))

