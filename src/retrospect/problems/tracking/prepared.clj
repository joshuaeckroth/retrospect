(ns retrospect.problems.tracking.prepared
  (:use [retrospect.problems.tracking.grid :only
         [new-grid grid-add grid-move grid-at
          update-all-entities find-entity]])
  (:use [retrospect.problems.tracking.truedata :only
         [generate-truedata]])
  (:use [retrospect.problems.tracking.sensors :only
         [new-sensor generate-sensors]])
  (:use [retrospect.colors])
  (:use [retrospect.random :only [set-seed]]))

(def basic-params
  {:GridHeight 10, :GridWidth 10, :MaxWalk 1, :Lazy false,
   :SensorNoise 0, :SensorSeesColor 100, :SensorCoverage 100,
   :BeliefNoise 0, :StepsBetween 1, :Steps 50,
   :ProbNewEntities 0, :NumberEntities 1, :MetaReasoning false,
   :Threshold 0})

(defn build-truedata
  [params paths]
  (let [width (:GridWidth params)
        height (:GridHeight params)
        steps (:Steps params)]
    (loop [t 0
           td []]
      (if (= t steps) td
          (let [g (update-all-entities (last td) t)
                g-new-es
                (reduce (fn [grid e]
                          (let [{:keys [x y color]} (meta e)
                                g (with-meta grid
                                    (assoc-in (meta grid) [:movements e]
                                              {:e e :ox x :oy y :ot t
                                               :x x :y y :t t :color color}))]
                            (grid-add g (:x (meta e)) (:y (meta e)) e)))
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
                                     (keys paths))))]
            (recur (inc t) (conj td g-moves)))))))

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

(defn simple-disappearance
  []
  (let [params (merge basic-params {:Steps 2 :SensorCoverage 50
                                    :SensorSeesColor 0})]
    {:params params
     :sensors [(new-sensor (keyword "right") 5 9 0 9 false)]
     :truedata (build-truedata
                params (entity-paths ["1" red  0 5,4 4,4]
                                     ["2" blue 0 5,5 5,4]))}))

(defn color-update
  []
  (let [params (merge basic-params {:Steps 3 :StepsBetween 3 :MaxWalk 10})]
    {:params params
     :sensors [(new-sensor (keyword "1") 0 3 0 9 false)
               (new-sensor (keyword "2g") 4 7 0 9 true)
               (new-sensor (keyword "3") 8 9 0 9 false)]
     :truedata (build-truedata
                params (entity-paths ["1" red 0 9,5 5,5 2,5]))}))

(defn color-update-2
  []
  (let [{params :params sensors :sensors truedata :truedata} color-update]
    {:params (assoc params :StepsBetween 1)
     :sensors sensors
     :truedata truedata}))

(defn gray-in-range
  []
  (let [params (merge basic-params {:Steps 3 :MaxWalk 10})]
    {:params params
     :sensors [(new-sensor (keyword "top") 0 9 0 3 true)
               (new-sensor (keyword "middle-gray") 0 9 4 6 false)
               (new-sensor (keyword "bottom") 0 9 7 9 true)]
     :truedata (build-truedata
                params (entity-paths ["1" red  0 3,0 3,5 3,9]
                                     ["2" blue 0 7,0 7,5 7,9]))}))

(defn intersection-ambiguity
  []
  (let [params (merge basic-params {:Steps 4 :SensorSeesColor 80 :MaxWalk 3
                                    :MetaStrategy :BatchBeginning})]
    {:params params
     :sensors [(new-sensor (keyword "left") 0 2 0 9 true)
               (new-sensor (keyword "middle") 3 4 0 9 false)
               (new-sensor (keyword "right") 5 9 0 9 true)]
     :truedata (build-truedata
                params (entity-paths ["1" red  0 5,7 4,4 2,3 0,3]
                                     ["2" blue 0 5,4 4,7 2,8 0,8]))}))

(defn intersection-ambiguity-nometa
  []
  (assoc-in intersection-ambiguity [:params :MetaStrategy] :NoMetareasoning))

(defn intersection-ambiguity-nometa-allatonce
  []
  (-> (intersection-ambiguity)
      (assoc-in [:params :MetaAbduction] false)
      (assoc-in [:params :StepsBetween] 4)))

(defn intersection-ambiguity-long
  []
  (let [params (merge basic-params {:Steps 5 :SensorSeesColor 80 :MaxWalk 10
                                    :MetaStrategy :BatchBeginning
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
  (let [params (merge basic-params {:Steps 6 :MaxWalk 1 :MetaStrategy :BatchBeginning})]
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
                 "gray-in-range" gray-in-range
                 "intersect" intersection-ambiguity
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
