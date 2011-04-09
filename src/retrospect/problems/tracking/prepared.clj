(ns retrospect.problems.tracking.prepared
  (:use [retrospect.problems.tracking.grid :only
         [new-grid grid-add grid-move grid-at update-all-entity-times find-entity]])
  (:use [retrospect.problems.tracking.sensors :only [new-sensor]])
  (:use [retrospect.colors]))

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
                (reduce (fn [grid e] (grid-add grid (:x (meta e)) (:y (meta e)) e))
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
                             (filter (fn [e] (< (:time (meta e)) t)) (keys paths))))
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
                     (map (fn [pair] {:x (first pair) :y (second pair)}) xy-pairs))]
    [(new-entity id (ffirst xy-pairs) (second (first xy-pairs)) color time)
      path]))

(defn entity-paths
  [& eps]
  (let [es-with-paths (apply concat (map entity-path eps))]
    (apply hash-map es-with-paths)))

(def simple-disappearance
  (let [params (merge basic-params {:Steps 2 :SensorCoverage 50 :SensorSeesColor 0})]
    {:params params
     :sensors [(new-sensor (keyword "right") 5 9 0 9 false)]
     :truedata (build-truedata params (entity-paths ["1" red  0 5,4 4,4]
                                                    ["2" blue 0 5,5 5,4]))}))

(def color-update
  (let [params (merge basic-params {:Steps 3 :StepsBetween 3 :MaxWalk 10})]
    {:params params
     :sensors [(new-sensor (keyword "1") 0 3 0 9 false)
               (new-sensor (keyword "2g") 4 7 0 9 true)
               (new-sensor (keyword "3") 8 9 0 9 false)]
     :truedata (build-truedata params (entity-paths ["1" red 0 9,5 5,5 2,5]))}))

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
        :truedata (build-truedata params (entity-paths ["1" red  0 3,0 3,5 3,9]
                                                       ["2" blue 0 7,0 7,5 7,9]))}))

(def intersection-ambiguity
  (let [params (merge basic-params {:Steps 3 :SensorSeesColor 80 :MaxWalk 3
                                    :MetaAbduction true})]
    {:params params
     :sensors [(new-sensor (keyword "left") 0 2 0 9 true)
               (new-sensor (keyword "middle") 3 4 0 9 false)
               (new-sensor (keyword "right") 5 9 0 9 true)]
     :truedata (build-truedata params (entity-paths ["1" red  0 5,7 4,4 2,3]
                                                    ["2" blue 0 5,4 4,7 2,8]))}))

(def intersection-ambiguity-nometa
  (assoc-in intersection-ambiguity [:params :MetaAbduction] false))

(def intersection-ambiguity-nometa-allatonce
  (-> intersection-ambiguity
      (assoc-in [:params :MetaAbduction] false)
      (assoc-in [:params :StepsBetween] 3)))

(def split-ambiguity
  (let [params (merge basic-params {:Steps 4 :MaxWalk 2})]
    {:params params
     :sensors [(new-sensor (keyword "x") 0 9 0 9 true)]
     :truedata (build-truedata params (entity-paths ["1" red 0 4,0 4,2 6,4 6,6]
                                                    ["2" red 0 4,0 4,2 2,4 2,6]))}))

(def split-ambiguity-2
  (let [params (merge basic-params {:Steps 4 :MaxWalk 1})]
    {:params params
     :sensors [(new-sensor (keyword "x") 0 9 0 9 true)]
     :truedata (build-truedata params (entity-paths ["1" red  0 1,0 1,1 0,2 0,3]
                                                    ["2" red  0 1,0 1,1 2,2 2,3]
                                                    ["3" blue 0 6,0 6,1 5,2 5,3]
                                                    ["4" blue 0 6,0 6,1 7,2 7,3]))}))

(def merge-ambiguity
  (let [params (merge basic-params {:Steps 4 :MaxWalk 3})]
    {:params params
     :sensors [(new-sensor (keyword "x") 0 9 0 9 true)]
     :truedata (build-truedata params (entity-paths ["1" red 0 4,0 4,2 5,4 5,6]
                                                    ["2" red 0 6,0 6,2 5,4 5,6]))}))

(def merge-ambiguity-2
  (let [params (merge basic-params {:Steps 4 :MaxWalk 3})]
    {:params params
     :sensors [(new-sensor (keyword "x") 0 9 0 9 true)]
     :truedata (build-truedata params (entity-paths ["1" red  0 0,0 0,2 1,4 1,6]
                                                    ["2" red  0 2,0 2,2 1,4 1,6]
                                                    ["3" blue 0 7,0 7,2 8,4 8,6]
                                                    ["4" blue 0 9,0 9,2 8,4 8,6]))}))

(def merge-ambiguity-gray
     (let [params (merge basic-params {:Steps 6 :MaxWalk 3})]
       {:params params
        :sensors [(new-sensor (keyword "top") 0 9 0 3 true)
                  (new-sensor (keyword "middle-gray") 0 9 4 6 false)
                  (new-sensor (keyword "bottom") 0 9 7 9 true)]
        :truedata (build-truedata params
                                  (entity-paths ["1" red  0 3,0 3,2 5,4 5,6 3,8 3,9]
                                                ["2" blue 0 7,0 7,2 5,4 5,6 7,8 7,9]))}))

(def split-merge
  (let [params (merge basic-params {:Steps 6 :MaxWalk 2})]
    {:params params
     :sensors [(new-sensor (keyword "x") 0 9 0 9 true)]
     :truedata (build-truedata params (entity-paths ["1" red 0 5,0 5,1 7,3 7,4 5,5 5,6]
                                                    ["2" red 0 5,0 5,1 3,3 3,4 5,5 5,6]))}))

(def split-merge-allatonce
  (assoc-in split-merge [:params :StepsBetween] 6))

(def split-non-ambiguity
  (let [params (merge basic-params {:Steps 4})]
    {:params params
     :sensors [(new-sensor (keyword "x") 0 9 0 9 true)]
     :truedata (build-truedata params (entity-paths ["1" red  0 4,3 4,4 5,5 5,6]
                                                    ["2" blue 0 4,3 4,4 3,5 3,6]))}))

(def split-merge-gray
     (let [params (merge basic-params {:Steps 6 :MaxWalk 5})]
       {:params params
        :sensors [(new-sensor (keyword "top") 0 9 0 3 true)
                  (new-sensor (keyword "middle-gray") 0 9 4 6 false)
                  (new-sensor (keyword "bottom") 0 9 7 9 true)]
        :truedata (build-truedata params
                                  (entity-paths ["1" red  0 3,0 3,3 5,4 5,6 3,8 3,9]
                                                ["2" blue 0 5,0 5,3 5,4 5,6 7,8 7,9]
                                                ["3" red  0 7,0 7,3 5,4 5,6 5,8 5,9]))}))

(def march
  (let [params (merge basic-params {:Steps 6 :MaxWalk 1 :MetaAbduction true})]
    {:params params
     :sensors [(new-sensor (keyword "x") 0 9 0 9 true)]
     :truedata (build-truedata params (entity-paths ["1" red 0 0,5 1,5 2,5 3,5 4,5 5,5]
                                                    ["2" red 0 1,5 2,5 3,5 4,5 5,5 6,5]
                                                    ["3" red 0 2,5 3,5 4,5 5,5 6,5 7,5]
                                                    ["4" red 0 3,5 4,5 5,5 6,5 7,5 8,5]
                                                    ["5" red 0 4,5 5,5 6,5 7,5 8,5 9,5]))}))

(def prepared-map
  (sorted-map "color-update" color-update
              "color-update-2" color-update-2
              "gray-in-range" gray-in-range
              "intersect" intersection-ambiguity
              "intersect-nom" intersection-ambiguity-nometa
              "intersect-nom-aao" intersection-ambiguity-nometa-allatonce
              "march" march
              "merge" merge-ambiguity
              "merge-2" merge-ambiguity-2
              "merge-gray" merge-ambiguity-gray
              "simple-dis" simple-disappearance
              "split" split-ambiguity
              "split-2" split-ambiguity-2
              "split-merge" split-merge
              "split-merge-aao" split-merge-allatonce
              "split-merge-gray" split-merge-gray
              "split-non-am" split-non-ambiguity))
