(ns retrospect.problems.words.prepared
  (:use [retrospect.problems.words.truedata :only [generate-truedata]])
  (:use [retrospect.problems.words.sensors :only [generate-sensors]])
  (:use [retrospect.random :only [set-seed]]))

(defn random-1
  []
  (let [params {:Steps 500 :StepsBetween 20 :Threshold 25 :SensorNoise 20
                :BeliefNoise 0 :MaxModelGrams 3
                :Seed 10 :Control "meta,!trans,lazy"}]
    (set-seed 10)
    {:params params
     :truedata (generate-truedata params)
     :sensors (generate-sensors params)}))

(def prepared-map
     (sorted-map "random-1" random-1))
