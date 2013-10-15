(ns retrospect.sensors)

(defrecord Sensor
    [id
     sense-fn
     sensed
     sensed-up-to])

(defn init-sensor
  [id sense-fn meta]
  (with-meta (Sensor. id sense-fn (atom {}) -1) meta))

(defn sensed-at
  [sensor time]
  (or (get-in @(:sensed sensor) [time :observed]) []))

(defn sense-more-at
  [sensor time]
  (swap! (:sensed sensor) (fn [m] (update-in m [time :observed] concat (get-in m [time :reserved]))))
  (sensed-at sensor time))

(defn add-sensed
  [sensor time observed reserved]
  (swap! (:sensed sensor) (fn [m] (assoc-in m [time] {:observed observed :reserved reserved})))
  (assoc sensor :sensed-up-to time))

(defn update-sensors
  "Don't resense already sensed time steps."
  [sensors test time]
  (doall (map (fn [s] (if (<= time (:sensed-up-to s)) s
                          ((:sense-fn s) s test time)))
              sensors)))

(defn reset-sensors
  [sensors]
  (map #(assoc % :sensed {}) sensors))
