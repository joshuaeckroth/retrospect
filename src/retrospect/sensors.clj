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
  (or (get-in (:sensed sensor) [time :observed]) []))

(defn sense-more-at
  [sensor time]
  (or (get-in (:sensed sensor) [time :reserved]) []))

(defn add-sensed
  [sensor time observed reserved]
  (-> sensor
      (update-in [:sensed] assoc time {:observed observed :reserved reserved})
      (assoc :sensed-up-to time)))

(defn update-sensors
  "Don't resense already sensed time steps."
  [sensors test time]
  (doall (map (fn [s] (if (<= time (:sensed-up-to s)) s
                          ((:sense-fn s) s test time)))
              sensors)))

(defn reset-sensors
  [sensors]
  (map #(assoc % :sensed {}) sensors))
