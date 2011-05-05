(ns retrospect.problems.tracking.prepared
  (:use [retrospect.problems.tracking.grid :only
         [new-grid grid-add grid-move grid-at
          update-all-entity-times find-entity]])
  (:use [retrospect.problems.tracking.truedata :only
         [generate-truedata]])
  (:use [retrospect.problems.tracking.sensors :only
         [new-sensor generate-sensors]])
  (:use [retrospect.colors])
  (:use [retrospect.random :only [set-seed]]))

(def basic-params
  {:GridHeight 10, :GridWidth 10, :MaxWalk 1, :Lazy false, :ProbMovement 50,
   :SensorNoise 0, :SensorSeesColor 100, :SensorCoverage 100,
   :BeliefNoise 0, :StepsBetween 1, :Steps 50,
   :ProbNewEntities 0, :NumberEntities 1, :MetaAbduction false})

(defn build-truedata
  [params paths]
  (let [width (:GridWidth params)
        height (:GridHeight params)
        steps (:Steps params)]
    (loop [t 0
           td []]
      (if (= t steps) td
          (let [g (last td)
                g-new-es
                (reduce (fn [grid e]
                          (grid-add grid (:x (meta e)) (:y (meta e)) e))
                        (if (= t 0) (new-grid width height) g)
                        (filter (fn [e] (= (:time (meta e)) t)) (keys paths)))
                g-moves
                (reduce (fn [g {e :e x :x y :y ox :ox oy :oy}]
                          (grid-move g (find-entity g e) x y))
                        g-new-es
                        (map (fn [e] {:e e :x (:x (get (paths e) t))
                                      :y (:y (get (paths e) t))
                                      :ox (:x (get (paths e) (dec t)))
                                      :oy (:y (get (paths e) (dec t)))})
                             (filter (fn [e] (< (:time (meta e)) t))
                                     (keys paths))))
                g-time (update-all-entity-times g-moves t)]
            (recur (inc t) (conj td g-time)))))))

(defn new-entity
  [id x y color time]
  (with-meta (symbol (str id))
    {:x x :y y :color color :time time}))

(defn entity-path
  [[id color time & xys]]
  (let [xy-pairs (partition 2 xys)
        path (zipmap (range time (+ time (count xy-pairs)))
                     (map (fn [pair] {:x (first pair) :y (second pair)})
                          xy-pairs))]
    [(new-entity id (ffirst xy-pairs) (second (first xy-pairs)) color time)
     path]))

(defn entity-paths
  [& eps]
  (let [es-with-paths (apply concat (map entity-path eps))]
    (apply hash-map es-with-paths)))

(def simple-disappearance
     (let [params (merge basic-params {:Steps 2 :SensorCoverage 50
                                       :SensorSeesColor 0})]
       {:params params
        :sensors [(new-sensor (keyword "right") 5 9 0 9 false)]
        :truedata (build-truedata
                   params (entity-paths ["1" red  0 5,4 4,4]
                                        ["2" blue 0 5,5 5,4]))}))

(def color-update
     (let [params (merge basic-params {:Steps 3 :StepsBetween 3 :MaxWalk 10})]
       {:params params
        :sensors [(new-sensor (keyword "1") 0 3 0 9 false)
                  (new-sensor (keyword "2g") 4 7 0 9 true)
                  (new-sensor (keyword "3") 8 9 0 9 false)]
        :truedata (build-truedata
                   params (entity-paths ["1" red 0 9,5 5,5 2,5]))}))

(def color-update-2
     (let [{params :params sensors :sensors truedata :truedata} color-update]
       {:params (assoc params :StepsBetween 1)
        :sensors sensors
        :truedata truedata}))

(def gray-in-range
     (let [params (merge basic-params {:Steps 3 :MaxWalk 10})]
       {:params params
        :sensors [(new-sensor (keyword "top") 0 9 0 3 true)
                  (new-sensor (keyword "middle-gray") 0 9 4 6 false)
                  (new-sensor (keyword "bottom") 0 9 7 9 true)]
        :truedata (build-truedata
                   params (entity-paths ["1" red  0 3,0 3,5 3,9]
                                        ["2" blue 0 7,0 7,5 7,9]))}))

(def intersection-ambiguity
     (let [params (merge basic-params {:Steps 4 :SensorSeesColor 80 :MaxWalk 3
                                       :MetaAbduction true})]
       {:params params
        :sensors [(new-sensor (keyword "left") 0 2 0 9 true)
                  (new-sensor (keyword "middle") 3 4 0 9 false)
                  (new-sensor (keyword "right") 5 9 0 9 true)]
        :truedata (build-truedata
                   params (entity-paths ["1" red  0 5,7 4,4 2,3 0,3]
                                        ["2" blue 0 5,4 4,7 2,8 0,8]))}))

(def intersection-ambiguity-nometa
     (assoc-in intersection-ambiguity [:params :MetaAbduction] false))

(def intersection-ambiguity-nometa-allatonce
     (-> intersection-ambiguity
         (assoc-in [:params :MetaAbduction] false)
         (assoc-in [:params :StepsBetween] 4)))

(def intersection-ambiguity-many
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

(def intersection-ambiguity-many-allatonce
     (assoc-in intersection-ambiguity-many [:params :StepsBetween] 10))

(def split-ambiguity
     (let [params (merge basic-params {:Steps 4 :MaxWalk 2})]
       {:params params
        :sensors [(new-sensor (keyword "x") 0 9 0 9 true)]
        :truedata (build-truedata
                   params (entity-paths ["1" red 0 4,0 4,2 6,4 6,6]
                                        ["2" red 0 4,0 4,2 2,4 2,6]))}))

(def split-ambiguity-2
     (let [params (merge basic-params {:Steps 4 :MaxWalk 1})]
       {:params params
        :sensors [(new-sensor (keyword "x") 0 9 0 9 true)]
        :truedata (build-truedata
                   params (entity-paths ["1" red  0 1,0 1,1 0,2 0,3]
                                        ["2" red  0 1,0 1,1 2,2 2,3]
                                        ["3" blue 0 6,0 6,1 5,2 5,3]
                                        ["4" blue 0 6,0 6,1 7,2 7,3]))}))

(def merge-ambiguity
     (let [params (merge basic-params {:Steps 4 :MaxWalk 3})]
       {:params params
        :sensors [(new-sensor (keyword "x") 0 9 0 9 true)]
        :truedata (build-truedata
                   params (entity-paths ["1" red 0 4,0 4,2 5,4 5,6]
                                        ["2" red 0 6,0 6,2 5,4 5,6]))}))

(def merge-ambiguity-2
     (let [params (merge basic-params {:Steps 4 :MaxWalk 3})]
       {:params params
        :sensors [(new-sensor (keyword "x") 0 9 0 9 true)]
        :truedata (build-truedata
                   params (entity-paths ["1" red  0 0,0 0,2 1,4 1,6]
                                        ["2" red  0 2,0 2,2 1,4 1,6]
                                        ["3" blue 0 7,0 7,2 8,4 8,6]
                                        ["4" blue 0 9,0 9,2 8,4 8,6]))}))

(def merge-ambiguity-gray
     (let [params (merge basic-params {:Steps 6 :MaxWalk 3})]
       {:params params
        :sensors [(new-sensor (keyword "top") 0 9 0 3 true)
                  (new-sensor (keyword "middle-gray") 0 9 4 6 false)
                  (new-sensor (keyword "bottom") 0 9 7 9 true)]
        :truedata (build-truedata
                   params (entity-paths ["1" red  0 3,0 3,2 5,4 5,6 3,8 3,9]
                                        ["2" blue 0 7,0 7,2 5,4 5,6 7,8 7,9]))}))

(def split-merge
     (let [params (merge basic-params {:Steps 6 :MaxWalk 2})]
       {:params params
        :sensors [(new-sensor (keyword "x") 0 9 0 9 true)]
        :truedata (build-truedata
                   params (entity-paths ["1" red 0 5,0 5,1 7,3 7,4 5,5 5,6]
                                        ["2" red 0 5,0 5,1 3,3 3,4 5,5 5,6]))}))

(def split-merge-allatonce
     (assoc-in split-merge [:params :StepsBetween] 6))

(def split-non-ambiguity
     (let [params (merge basic-params {:Steps 4})]
       {:params params
        :sensors [(new-sensor (keyword "x") 0 9 0 9 true)]
        :truedata (build-truedata
                   params (entity-paths ["1" red  0 4,3 4,4 5,5 5,6]
                                        ["2" blue 0 4,3 4,4 3,5 3,6]))}))

(def split-merge-gray
     (let [params (merge basic-params {:Steps 6 :MaxWalk 5})]
       {:params params
        :sensors [(new-sensor (keyword "top") 0 9 0 3 true)
                  (new-sensor (keyword "middle-gray") 0 9 4 6 false)
                  (new-sensor (keyword "bottom") 0 9 7 9 true)]
        :truedata (build-truedata
                   params (entity-paths ["1" red  0 3,0 3,3 5,4 5,6 3,8 3,9]
                                        ["2" blue 0 5,0 5,3 5,4 5,6 7,8 7,9]
                                        ["3" red  0 7,0 7,3 5,4 5,6 5,8 5,9]))}))

(def march
     (let [params (merge basic-params {:Steps 6 :MaxWalk 1 :MetaAbduction true})]
       {:params params
        :sensors [(new-sensor (keyword "x") 0 9 0 9 true)]
        :truedata (build-truedata
                   params (entity-paths ["1" red 0 0,5 1,5 2,5 3,5 4,5 5,5]
                                        ["2" red 0 1,5 2,5 3,5 4,5 5,5 6,5]
                                        ["3" red 0 2,5 3,5 4,5 5,5 6,5 7,5]
                                        ["4" red 0 3,5 4,5 5,5 6,5 7,5 8,5]
                                        ["5" red 0 4,5 5,5 6,5 7,5 8,5 9,5]))}))

(def march-aao
     (assoc-in march [:params :StepsBetween] 6))

(def random-1
     (let [params {:GridHeight 30 :MaxWalk 10 :SensorNoise 0
                   :SensorSeesColor 60 :SensorCoverage 100 :BeliefNoise 0
                   :StepsBetween 1 :GridWidth 30 :Steps 30
                   :ProbNewEntities 0 :NumberEntities 10
                   :MetaAbduction false :Lazy false}]
       {:params params
        :sensors [(new-sensor (keyword "1") 0 4 0 29 true)
                  (new-sensor (keyword "1g") 5 9 0 29 false)
                  (new-sensor (keyword "2") 10 14 0 29 true)
                  (new-sensor (keyword "2g") 15 19 0 29 false)
                  (new-sensor (keyword "3") 20 24 0 29 true)
                  (new-sensor (keyword "3g") 25 29 0 29 false)]
        :truedata (build-truedata
                   params (entity-paths
                           ["6" blue 0 17,20 19,18 22,17 21,16 20,19 19,21
                            18,18 19,21 21,20 21,19 17,23 18,22 19,24 20,24
                            22,26 23,26 24,26 24,29 25,27 27,29 28,28 26,27
                            25,28 28,28 26,28 24,27 24,28 24,28 25,27 26,28 26,29]
                           ["7" red 0 22,0 18,5 18,5 20,3 21,4 20,5 20,3 19,4
                            20,6 20,3 22,1 22,0 19,0 17,3 18,3 21,7 22,8 20,13
                            21,13 20,14 20,13 21,13 20,13 20,12 21,11 20,10
                            21,10 22,9 23,9 22,7 22,4]
                           ["4" red 0 10,9 11,11 13,11 13,11 10,12 10,13 7,12
                            9,12 9,13 8,8 12,7 11,6 11,9 12,11 11,11 11,10 14,9
                            15,10 14,9 16,8 12,6 14,9 13,7 13,7 13,3 13,1 11,3
                            10,2 12,1 13,1 14,3]
                           ["5" blue 0 12,22 13,23 10,22 10,23 8,18 6,18 6,16
                            6,13 8,12 9,14 11,11 9,10 9,9 8,10 10,12 9,10 14,12
                            14,10 16,12 16,11 15,11 20,17 15,16 16,15 14,16
                            14,13 14,13 16,10 13,10 15,7 15,7]
                           ["1" blue 0 12,9 10,12 11,11 13,10 16,8 15,6 17,3
                            16,4 19,4 17,6 18,5 18,5 19,6 19,7 23,10 23,12
                            20,12 20,12 22,12 22,8 27,8 24,8 24,7 24,6 23,1 24,1
                            23,1 26,1 26,2 24,2 26,0]
                           ["0" red 0 8,4 5,5 5,6 6,6 8,6 8,8 7,6 8,9 9,6 11,4
                            11,3 11,2 12,2 13,3 12,2 15,0 13,1 10,2 10,4 9,7 10,6
                            11,4 10,3 9,3 8,2 10,1 10,0 8,3 8,3 9,6 12,5]
                           ["3" red 0 4,2 4,1 6,1 8,0 10,0 11,0 11,3 10,3 12,3
                            11,4 13,2 12,1 14,0 15,2 14,4 12,5 13,4 16,5 15,6
                            16,4 15,5 16,6 16,5 17,6 16,7 21,6 22,2 22,5 21,9
                            20,8 20,9]
                           ["2" blue 0 27,29 28,28 27,25 28,27 25,29 26,27
                            26,29 28,27 28,26 27,28 28,29 27,29 29,28 27,27
                            22,25 20,26 24,26 25,26 26,26 25,25 25,24 27,25
                            27,24 27,23 28,28 28,28 28,28 28,27 28,27 29,28 28,25]
                           ["9" blue 0 5,4 5,4 6,6 4,0 2,0 1,2 0,1 0,1 1,1 1,0
                            5,0 7,0 8,0 11,0 11,1 12,0 9,2 6,3 9,3 10,5 9,7 9,2
                            10,2 7,1 4,0 2,1 1,0 2,1 2,4 3,10 3,10]
                           ["8" red 0 9,23 9,23 8,28 11,26 10,27 9,27 9,26 8,27
                            6,27 6,28 3,26 1,29 3,29 4,29 3,26 8,27 9,28 9,27
                            10,27 8,28 8,25 7,25 7,26 8,29 5,28 2,27 2,27 4,27
                            3,25 3,24 4,26]))}))

(def random-1-sb4
     (assoc-in random-1 [:params :StepsBetween] 4))

(def random-2
     (let [params {:GridHeight 20, :GridWidth 20, :MaxWalk 10,
                   :Lazy false, :ProbMovement 50,
                   :SensorNoise 0, :SensorSeesColor 60,
                   :SensorCoverage 100, :BeliefNoise 0, :StepsBetween 4,
                   :Steps 50, :ProbNewEntities 0, :NumberEntities 5,
                   :MetaAbduction false}]
       (set-seed 10)
       {:params params
        :truedata (generate-truedata params)
        :sensors (generate-sensors params)}))

(def prepared-map
     (sorted-map "color-update" color-update
                 "color-update-2" color-update-2
                 "gray-in-range" gray-in-range
                 "intersect" intersection-ambiguity
                 "intersect-many" intersection-ambiguity-many
                 "intersect-many-aao" intersection-ambiguity-many-allatonce
                 "intersect-nom" intersection-ambiguity-nometa
                 "intersect-nom-aao" intersection-ambiguity-nometa-allatonce
                 "march" march
                 "march-aao" march-aao
                 "merge" merge-ambiguity
                 "merge-2" merge-ambiguity-2
                 "merge-gray" merge-ambiguity-gray
                 "random-1" random-1
                 "random-1-sb4" random-1-sb4
                 "random-2" random-2
                 "simple-dis" simple-disappearance
                 "split" split-ambiguity
                 "split-2" split-ambiguity-2
                 "split-merge" split-merge
                 "split-merge-aao" split-merge-allatonce
                 "split-merge-gray" split-merge-gray
                 "split-non-am" split-non-ambiguity))
