(ns retrospect.problems.tracking.test-truedata
  (:use [retrospect.random])
  (:use [retrospect.problems.tracking.truedata]))

(def basic-params
  {:GridHeight 3, :GridWidth 3, :MaxWalk 10, :Lazy false, :ProbMovement 100,
   :SensorNoise 0, :SensorSeesColor 100, :SensorCoverage 100,
   :BeliefNoise 0, :StepsBetween 1, :Steps 5,
   :ProbNewEntities 0, :NumberEntities 20, :MetaAbduction false})



