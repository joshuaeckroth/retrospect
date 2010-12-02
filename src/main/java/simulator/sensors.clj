(ns simulator.sensors)

(defrecord Sensor
    [sense-fn
     sensed
     sensed-up-to
     attributes])

(defn init-sensor
  [sense-fn attributes]
  (Sensor. sense-fn {} -1 attributes))

(defn sensed-at
  [sensor time]
  (get (:sensed sensor) time))

(defn add-sensed
  [sensor time data]
  (-> sensor
      (update-in [:sensed] assoc time data)
      (assoc :sensed-up-to time)))

(defn update-sensors
  [sensors moment time]
  "Don't resense already sensed time steps."
  (doall (map (fn [s] (if (<= time (:sensed-up-to s)) s
                          ((:sense-fn s) s moment time)))
              sensors)))

