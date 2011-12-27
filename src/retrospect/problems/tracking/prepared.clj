(ns retrospect.problems.tracking.prepared
  (:require [retrospect.state])
  (:require [retrospect.random])
  (:use [retrospect.random :only [new-seed]])
  (:use [retrospect.problems.tracking.truedata :only
         [generate-truedata]])
  (:use [retrospect.problems.tracking.sensors :only
         [new-sensor generate-sensors]])
  (:use [retrospect.problems.tracking.movements :only [new-movements move-entity]])
  (:use [retrospect.colors]))

(def basic-params
  {:GridHeight 10, :GridWidth 10, :MaxWalk 10, :Knowledge 100, :Seed 100,
   :SensorNoise 0, :SensorSeesColor 100, :SensorCoverage 100,
   :BeliefNoise 0, :StepsBetween 1, :Steps 50, :KnowBiases true,
   :ProbNewEntities 0, :NumberEntities 1, :MetaReasoning "NoMetareasoning",
   :TransitiveExplanation false, :Threshold 0})

(defn entity-paths
  [params & eps]
  (let [movements (reduce (fn [m [id bias c t x y & _]]
                            (assoc m (symbol id)
                                   [{:x x :y y :time t :color c :bias bias}]))
                          (new-movements (:GridWidth params) (:GridHeight params))
                          eps)]
    (reduce (fn [m [id _ _ start-time & xys]]
              (let [xy-pairs (rest (partition 2 xys))]
                (reduce (fn [m2 [x y t]] (move-entity m2 (symbol id) x y t))
                        m (map (fn [[t xy]] (conj (vec xy) (inc (+ start-time t))))
                               (partition 2 (interleave (range (count xy-pairs))
                                                        xy-pairs))))))
            movements eps)))

(defn prepared-from-params
  [params]
  (binding [retrospect.state/params params
            retrospect.random/rgen (new-seed (:Seed params))]
    {:params params :sensors (generate-sensors) :truedata (generate-truedata)}))

(defn simple-disappearance
  []
  (let [params (merge basic-params {:Steps 1 :SensorCoverage 50
                                    :SensorSeesColor 0})]
    {:params params
     :sensors [(new-sensor (keyword "right") 5 9 0 9 false)]
     :truedata (entity-paths params
                             ["1" :straight red  0 5,4 4,4]
                             ["2" :straight blue 0 5,5 5,4])}))

(defn color-update
  []
  (let [params (merge basic-params {:Steps 2 :StepsBetween 2})]
    {:params params
     :sensors [(new-sensor (keyword "1") 0 3 0 9 false)
               (new-sensor (keyword "2g") 4 7 0 9 true)
               (new-sensor (keyword "3") 8 9 0 9 false)]
     :truedata (entity-paths params ["1" :straight red 0 9,5 5,5 2,5])}))

(defn gray-in-range
  []
  (let [params (merge basic-params {:Steps 2})]
    {:params params
     :sensors [(new-sensor (keyword "top") 0 9 0 3 true)
               (new-sensor (keyword "middle-gray") 0 9 4 6 false)
               (new-sensor (keyword "bottom") 0 9 7 9 true)]
     :truedata (entity-paths params
                             ["1" :straight red   0 3,0 3,5 3,9]
                             ["2" :straight blue 0 7,0 7,5 7,9])}))

(defn intersection-ambiguity
  []
  (let [params (merge basic-params {:Steps 3 :SensorSeesColor 80
                                    :MetaReasoning "BatchBeginning" :MaxWalk 3
                                    :KnowBiases false})]
    {:params params
     :sensors [(new-sensor (keyword "left") 0 2 0 9 true)
               (new-sensor (keyword "middle") 3 4 0 9 false)
               (new-sensor (keyword "right") 5 9 0 9 true)]
     :truedata (entity-paths params
                             ["1" :left red  0 5,7 4,4 3,3 2,2]
                             ["2" :right blue 0 5,4 4,7 3,8 2,9])}))

(defn intersection-continued-ambiguity
  []
  (let [params (merge basic-params {:Steps 5 :SensorSeesColor 20
                                    :MetaReasoning "NoMetareasoning" :MaxWalk 3})]
    {:params params
     :sensors [(new-sensor :top 0 9 0 2 true)
               (new-sensor :bottom 0 9 3 9 false)]
     :truedata (entity-paths params
                             ["1" :straight red  0 2,0 2,1 3,2 3,3 2,4 1,5 0,6 0,7]
                             ["2" :straight blue 0 7,0 7,1 6,2 6,3 7,4 8,5 9,6 9,7])}))

(defn random-bias-bug-nometa
  []
  (prepared-from-params
   {:GridHeight 20, :MaxWalk 10, :TransitiveExplanation true,
    :Seed 4872271, :MetaReasoning "NoMetareasoning", :SensorNoise 0,
    :SensorSeesColor 60, :SensorCoverage 100, :BeliefNoise 0,
    :StepsBetween 3, :GridWidth 20, :Steps 18, :ProbNewEntities 0,
    :Threshold 0, :NumberEntities 4}))

(defn random-bug
  []
  (prepared-from-params
   {:GridHeight 20, :MaxWalk 10, :TransitiveExplanation true,
    :Seed 6186076, :MetaReasoning "NoMetareasoning", :SensorNoise 0,
    :SensorSeesColor 60, :SensorCoverage 100, :BeliefNoise 0,
    :StepsBetween 3, :GridWidth 20, :Steps 18, :ProbNewEntities 0,
    :Threshold 60, :NumberEntities 4}))

(comment
  (defn intersection-ambiguity-nometa
    []
    (assoc-in (intersection-ambiguity) [:params :MetaReasoning] true))

  (defn intersection-ambiguity-nometa-allatonce
    []
    (-> (intersection-ambiguity)
        (assoc-in [:params :MetaReasoning] false)
        (assoc-in [:params :StepsBetween] 4)))

  (defn intersection-ambiguity-long
    []
    (let [params (merge basic-params {:Steps 5 :SensorSeesColor 80 :MaxWalk 10
                                      :MetaReasoning true
                                      :GridWidth 20 :GridHeight 20})]
      {:params params
       :sensors [(new-sensor (keyword "left") 0 4 0 19 true)
                 (new-sensor (keyword "middle") 5 15 0 19 false)
                 (new-sensor (keyword "right") 16 19 0 19 true)]
       :truedata (build-truedata
                  params (entity-paths
                          ["1" red  0 18,15 15,4  10,1  5,1  3,1]
                          ["2" blue 0 18,1  15,15 10,18 5,18 3,18]))}))

  (defn intersection-ambiguity-many
    []
    (let [params (merge basic-params {:Steps 10 :SensorSeesColor 80 :MaxWalk 15
                                      :StepsBetween 1 :GridWidth 30
                                      :GridHeight 30})]
      {:params params
       :sensors [(new-sensor (keyword "1") 0 4 0 29 true)
                 (new-sensor (keyword "1g") 5 9 0 29 false)
                 (new-sensor (keyword "2") 10 14 0 29 true)
                 (new-sensor (keyword "2g") 15 19 0 29 false)
                 (new-sensor (keyword "3") 20 24 0 29 true)
                 (new-sensor (keyword "3g") 25 29 0 29 false)]
       :truedata
       (build-truedata
        params (entity-paths
                ["1" red  0 29,0 27,3 24,5 20,3 15,3 14,2 10,4 11,1 5,0 0,0]
                ["2" blue 0 29,2 23,3 20,6 20,2 18,7 14,2 11,8 9,9 4,5 0,4]
                ["3" red  0 29,4 25,5 22,4 19,4 16,8 10,8 11,7 6,8 3,0 0,6]))}))

  (defn intersection-ambiguity-many-allatonce
    []
    (assoc-in (intersection-ambiguity-many) [:params :StepsBetween] 10))

  (defn split-ambiguity
    []
    (let [params (merge basic-params {:Steps 4 :MaxWalk 2})]
      {:params params
       :sensors [(new-sensor (keyword "x") 0 9 0 9 true)]
       :truedata (build-truedata
                  params (entity-paths ["1" red 0 4,0 4,2 6,4 6,6]
                                       ["2" red 0 4,0 4,2 2,4 2,6]))}))

  (defn split-ambiguity-2
    []
    (let [params (merge basic-params {:Steps 4 :MaxWalk 1})]
      {:params params
       :sensors [(new-sensor (keyword "x") 0 9 0 9 true)]
       :truedata (build-truedata
                  params (entity-paths ["1" red  0 1,0 1,1 0,2 0,3]
                                       ["2" red  0 1,0 1,1 2,2 2,3]
                                       ["3" blue 0 6,0 6,1 5,2 5,3]
                                       ["4" blue 0 6,0 6,1 7,2 7,3]))}))

  (defn split0
    []
    (let [params (merge basic-params {:Steps 3 :MaxWalk 3})]
      {:params params
       :sensors [(new-sensor (keyword "x") 0 9 0 9 true)]
       :truedata (build-truedata
                  params (entity-paths ["1" red 0 3,3 4,6 4,8]
                                       ["2" red 0 3,3 2,6 2,8]))}))

  (defn merge0
    []
    (let [params (merge basic-params {:Steps 3 :MaxWalk 3})]
      {:params params
       :sensors [(new-sensor (keyword "x") 0 9 0 9 true)]
       :truedata (build-truedata
                  params (entity-paths ["1" red 0 5,3 3,6 3,8]
                                       ["2" red 0 2,3 3,6 3,8]))}))

  (defn merge-ambiguity
    []
    (let [params (merge basic-params {:Steps 4 :MaxWalk 3})]
      {:params params
       :sensors [(new-sensor (keyword "x") 0 9 0 9 true)]
       :truedata (build-truedata
                  params (entity-paths ["1" red 0 4,0 4,2 5,4 5,6]
                                       ["2" red 0 6,0 6,2 5,4 5,6]))}))

  (defn merge-ambiguity-2
    []
    (let [params (merge basic-params {:Steps 4 :MaxWalk 3})]
      {:params params
       :sensors [(new-sensor (keyword "x") 0 9 0 9 true)]
       :truedata (build-truedata
                  params (entity-paths ["1" red  0 0,0 0,2 1,4 1,6]
                                       ["2" red  0 2,0 2,2 1,4 1,6]
                                       ["3" blue 0 7,0 7,2 8,4 8,6]
                                       ["4" blue 0 9,0 9,2 8,4 8,6]))}))

  (defn merge-ambiguity-gray
    []
    (let [params (merge basic-params {:Steps 6 :MaxWalk 3})]
      {:params params
       :sensors [(new-sensor (keyword "top") 0 9 0 3 true)
                 (new-sensor (keyword "middle-gray") 0 9 4 6 false)
                 (new-sensor (keyword "bottom") 0 9 7 9 true)]
       :truedata (build-truedata
                  params (entity-paths ["1" red  0 3,0 3,2 5,4 5,6 3,8 3,9]
                                       ["2" blue 0 7,0 7,2 5,4 5,6 7,8 7,9]))}))

  (defn split-merge
    []
    (let [params (merge basic-params {:Steps 6 :MaxWalk 2})]
      {:params params
       :sensors [(new-sensor (keyword "x") 0 9 0 9 true)]
       :truedata (build-truedata
                  params (entity-paths ["1" red 0 5,0 5,1 7,3 7,4 5,5 5,6]
                                       ["2" red 0 5,0 5,1 3,3 3,4 5,5 5,6]))}))

  (defn split-merge-allatonce
    []
    (assoc-in (split-merge) [:params :StepsBetween] 6))

  (defn split-non-ambiguity
    []
    (let [params (merge basic-params {:Steps 4})]
      {:params params
       :sensors [(new-sensor (keyword "x") 0 9 0 9 true)]
       :truedata (build-truedata
                  params (entity-paths ["1" red  0 4,3 4,4 5,5 5,6]
                                       ["2" blue 0 4,3 4,4 3,5 3,6]))}))

  (defn split-merge-gray
    []
    (let [params (merge basic-params {:Steps 6 :MaxWalk 5})]
      {:params params
       :sensors [(new-sensor (keyword "top") 0 9 0 3 true)
                 (new-sensor (keyword "middle-gray") 0 9 4 6 false)
                 (new-sensor (keyword "bottom") 0 9 7 9 true)]
       :truedata (build-truedata
                  params (entity-paths ["1" red  0 3,0 3,3 5,4 5,6 3,8 3,9]
                                       ["2" blue 0 5,0 5,3 5,4 5,6 7,8 7,9]
                                       ["3" red  0 7,0 7,3 5,4 5,6 5,8 5,9]))}))

  (defn split-merge-twocolor
    []
    (let [params (merge basic-params {:Steps 6 :MaxWalk 3})]
      {:params params
       :sensors [(new-sensor (keyword "top") 0 9 0 3 true)
                 (new-sensor (keyword "middle-gray") 0 9 4 6 false)
                 (new-sensor (keyword "bottom") 0 9 7 9 true)]
       :truedata (build-truedata
                  params (entity-paths ["1" red   0 1,0 1,3 1,4 3,6 1,8 1,9]
                                       ["2" blue  0 5,0 5,3 5,4 5,6 7,8 7,9]
                                       ["3" green 0 7,0 7,3 7,4 5,6 5,8 5,9]))}))

  (defn split-merge-twocolor-aao
    []
    (assoc-in (split-merge-twocolor) [:params :StepsBetween] 6))

  (defn march
    []
    (let [params (merge basic-params {:Steps 6 :MaxWalk 1 :MetaReasoning true})]
      {:params params
       :sensors [(new-sensor (keyword "x") 0 9 0 9 true)]
       :truedata (build-truedata
                  params (entity-paths ["1" red 0 0,5 1,5 2,5 3,5 4,5 5,5]
                                       ["2" red 0 1,5 2,5 3,5 4,5 5,5 6,5]
                                       ["3" red 0 2,5 3,5 4,5 5,5 6,5 7,5]
                                       ["4" red 0 3,5 4,5 5,5 6,5 7,5 8,5]
                                       ["5" red 0 4,5 5,5 6,5 7,5 8,5 9,5]))}))

  (defn march-aao
    []
    (assoc-in (march) [:params :StepsBetween] 6))

  (def prepared-map
    (sorted-map "color-update" color-update
                "color-update-2" color-update-2
                "intersect-long" intersection-ambiguity-long
                "intersect-many" intersection-ambiguity-many
                "intersect-many-aao" intersection-ambiguity-many-allatonce
                "intersect-nom" intersection-ambiguity-nometa
                "intersect-nom-aao" intersection-ambiguity-nometa-allatonce
                "march" march
                "march-aao" march-aao
                "merge" merge-ambiguity
                "merge-2" merge-ambiguity-2
                "merge-gray" merge-ambiguity-gray
                "merge0" merge0
                "simple-dis" simple-disappearance
                "split" split-ambiguity
                "split0" split0
                "split-2" split-ambiguity-2
                "split-merge" split-merge
                "split-merge-aao" split-merge-allatonce
                "split-merge-gray" split-merge-gray
                "split-merge-twocolor" split-merge-twocolor
                "split-merge-twocolor-aao" split-merge-twocolor-aao
                "split-non-am" split-non-ambiguity))
  )

(def prepared-map
  (sorted-map "color-update" color-update
              "simple-dis" simple-disappearance
              "gray-in-range" gray-in-range
              "intersect" intersection-ambiguity
              "intersect-cont" intersection-continued-ambiguity
              "r-bias-bug" random-bias-bug-nometa
              "r-bug" random-bug))
