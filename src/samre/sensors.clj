(ns samre.sensors)

(defrecord Sensor
    [id
     sense-fn
     sensed
     sensed-up-to])

(defn init-sensor
  [id sense-fn meta]
  (with-meta (Sensor. id sense-fn {} -1) meta))

(defn sensed-from
  [sensor time]
  (apply concat (map (fn [t] (get (:sensed sensor) t))
                     (range time (inc (:sensed-up-to sensor))))))

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
