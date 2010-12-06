(ns simulator.player.state)

(def *problem* nil)
(def *params* nil)
(def *or-state* nil)
(def *sensors* nil)
(def *truedata* nil)
(def *time* 0)

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
