(ns retrospect.problems.abdexp.sensors
  (:use [retrospect.sensors :only [init-sensor add-sensed]])
  (:use [loom.graph :only [digraph]])
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [retrospect.random])
  (:use [retrospect.state]))

(defn sense
  [sensor test time]
  (let [observations (forced-nodes (or (get test time) (digraph)))]
    (if (= 0 (:SensorNoise params))
      (add-sensed sensor time observations)
      (let [noise-prob (/ (double (:SensorNoise params)) 100.0)
            noisy-observations (set (map (fn [v] (if (<= (my-rand) noise-prob)
                                                (format "O%d" (my-rand-int 1000)) v))
                                       observations))]
        (add-sensed sensor time noisy-observations)))))

(defn generate-sensors
  []
  [(init-sensor "expgraph" sense {})])
