(ns simulator.player.state)

(def *problem* nil)
(def *params* nil)
(def *or-state* nil)
(def *sensors* nil)
(def *truedata* nil)
(def *time* 0)
(def *time-prev* -1)

(defn update-problem
  [problem]
  (def *problem* problem))

(defn update-params
  [params]
  (def *params* params))

(defn update-or-state
  [ors]
  (def *or-state* ors))

(defn update-sensors
  [sensors]
  (def *sensors* sensors))

(defn update-truedata
  [truedata]
  (def *truedata* truedata))

(defn update-time
  [time]
  (def *time* time))

(defn update-time-prev
  [time]
  (if (nil? time) (def *time-prev* -1)
      (def *time-prev* time)))
