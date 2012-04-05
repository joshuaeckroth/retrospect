(ns retrospect.sensors)

(defrecord Sensor
    [id
     sense-fn
     sensed
     sensed-up-to])

(defn init-sensor
  [id sense-fn meta]
  (with-meta (Sensor. id sense-fn {} -1) meta))

(defn sensed-at
  [sensor time]
  (if-let [sensed (get (:sensed sensor) time)]
    sensed []))

(defn add-sensed
  [sensor time data]
  (-> sensor
      (update-in [:sensed] assoc time data)
      (assoc :sensed-up-to time)))

(defn update-sensors
  "Don't resense already sensed time steps."
  [sensors test time]
  (doall (map (fn [s] (if (<= time (:sensed-up-to s)) s
                          ((:sense-fn s) s test time)))
              sensors)))
