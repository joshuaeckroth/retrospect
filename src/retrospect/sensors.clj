(ns retrospect.sensors
  (:use [retrospect.state])
  (:use [geppetto.random]))

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
  (or (get-in @(:sensed sensor) [time]) []))

(defn add-sensed
  [sensor time observed]
  (doseq [obs observed]
    ;; possibly put this report at a later time, to simulate discovering
    ;; evidence later
    (let [new-time (if (< (my-rand) (/ (:ProbSensedLater params) 100.0))
                     (+ time (my-rand-int (- (:Steps params) time)))
                     time)]
      (swap! (:sensed sensor) (fn [m] (update-in m [new-time] conj obs)))))
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
