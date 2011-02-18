(ns samre.problems.tracking.prepared
  (:use [samre.problems.tracking.grid :only
         [new-grid grid-put grid-move grid-at update-all-entity-times]])
  (:use [samre.problems.tracking.sensors :only [new-sensor]])
  (:use [samre.colors]))

(def basic-params
  {:GridHeight 10, :GridWidth 10, :MaxWalk 1, :Lazy false, :ProbMovement 50,
   :SensorReportNoise 0, :SensorSeesColor 100, :SensorCoverage 100,
   :BeliefNoise 0, :StepsBetween 1, :Steps 50,
   :ProbNewEntities 0, :NumberEntities 1, :MetaAbduction false})

(defn build-truedata
  [width height steps paths]
  (loop [t 0
         td []]
    (if (> t steps) td
        (let [g (last td)
              g-new-es
              (reduce (fn [grid e] (grid-put grid (:x (meta e)) (:y (meta e)) e))
                      (if (= t 0) (new-grid width height) g)
                      (filter (fn [e] (= (:time (meta e)) t)) (keys paths)))
              g-moves
              (reduce (fn [g {e :e x :x y :y ox :ox oy :oy}]
                        (grid-move g (grid-at g ox oy) x y))
                      g-new-es
                      (map (fn [e] {:e e :x (:x (get (paths e) t))
                                    :y (:y (get (paths e) t))
                                    :ox (:x (get (paths e) (dec t)))
                                    :oy (:y (get (paths e) (dec t)))})
                           (filter (fn [e] (< (:time (meta e)) t)) (keys paths))))
              g-time (update-all-entity-times g-moves t)]
          (recur (inc t) (conj td g-time))))))

(defn new-entity
  [n x y color time]
  (with-meta (symbol (str n))
    {:x x :y y :color color :time time}))

(defn entity-path
  [[n color time & xys]]
  (let [xy-pairs (partition 2 xys)
        path (zipmap (range (count xy-pairs))
                     (map (fn [pair] {:x (first pair) :y (second pair)}) xy-pairs))]
    [(new-entity n (first (first xy-pairs)) (second (first xy-pairs)) color time)
      path]))

(defn entity-paths
  [& eps]
  (let [es-with-paths (apply concat (map entity-path eps))]
    (apply hash-map es-with-paths)))

(def simple-disappearance
  {:params (merge basic-params {:Steps 3} {:SensorCoverage 50})
   :sensors [(new-sensor (keyword "right") 5 9 0 9 true)]
   :truedata (build-truedata 10 10 2
                             (entity-paths [0 red 0 4,5 6,7 7,3]
                                           [1 red 2 2,2]))})
