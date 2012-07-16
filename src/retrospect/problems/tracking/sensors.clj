(ns retrospect.problems.tracking.sensors
  (:use [retrospect.random])
  (:use [retrospect.problems.tracking.colors])
  (:use [retrospect.sensors :only [init-sensor add-sensed]])
  (:require [clojure.contrib.math :as math])
  (:use [retrospect.problems.tracking.movements :only
         [entity-movements entities-at walk-rand]])
  (:use [retrospect.random])
  (:use [retrospect.state]))

(defn perturb
  [sensor]
  (letfn [(rand-alter [det] (let [[x y] (walk-rand [(:x det) (:y det)])]
                              (if (and (< (my-rand) (/ (:ProbPerturb params) 100.0))
                                       (< x (:GridWidth params)) (>= x 0)
                                       (< y (:GridHeight params)) (>= y 0))
                                (assoc det :x x :y y) det)))]
    (assoc sensor :sensed
           (reduce (fn [sensed time]
                     (assoc sensed time (map rand-alter (get sensed time))))
                   (:sensed sensor) (keys (:sensed sensor))))))

(defn sees
  [sensor x y]
  (and (>= x (:left (meta sensor))) (<= x (:right (meta sensor)))
       (>= y (:bottom (meta sensor))) (<= y (:top (meta sensor)))))

(defn find-spotted
  [sensor movements time]
  ;; not most efficient way to figure out what the sensor sees...
  (apply concat
         (for [x (range (:width (meta movements)))
               y (range (:height (meta movements)))
               :when (sees sensor x y)]
           (mapcat (fn [e] (map (fn [mov]
                               ;; turn the movement into a detection, and
                               ;; add the color (or gray)
                               (assoc (dissoc mov :ox :oy :ot :bias)
                                 :color (if (:sees-color (meta sensor))
                                          (:color mov) gray)))
                             (entity-movements movements e time time)))
                   (entities-at movements x y time)))))

(defn make-random-det
  [sensor time]
  (let [x (my-rand-int (:GridWidth params))
        y (my-rand-int (:GridHeight params))
        color (if (sees sensor x y) (rand-color) gray)]
    {:x x :y y :time time :color color}))

(defn insertion-noise
  [observations sensor time]
  ;; each observation has the form {:x # :y # :time # :color ""}
  (let [noise (repeatedly
               (my-rand-int (int (* (/ (:SensorInsertionNoise params) 100)
                                    (:Steps params))))
               #(make-random-det sensor time))]
    (concat observations
            ;; don't allow noise to occupy same space as other detections
            (filter (fn [{:keys [x y]}]
                 (not-any? (fn [{xx :x yy :y}] (and (= x xx) (= y yy)))
                           observations))
               noise))))

(defn deletion-noise
  [observations sensor time]
  ;; each observation has the form {:x # :y # :time # :color ""}
  (filter (fn [_] (>= (my-rand) (/ (double (:SensorDeletionNoise params)) 100.0)))
     observations))

(defn distortion-noise
  [observations sensor time]
  ;; each observation has the form {:x # :y # :time # :color ""}
  (loop [obs observations
         new-obs []]
    (cond (empty? obs) new-obs
          (>= (my-rand) (/ (double (:SensorDistortionNoise params)) 100.0))
          (recur (rest obs) (conj new-obs (first obs)))
          :else
          (let [det (make-random-det sensor time)]
            ;; don't allow noise to occupy same space as other detections
            (if (not-any? (fn [{xx :x yy :y}]
                            (and (= (:x det) xx) (= (:y det) yy)))
                          new-obs)
              (recur (rest obs) (conj new-obs det))
              (recur (rest obs) (conj new-obs (first obs))))))))

(defn sense
  [sensor movements time]
  (let [observations (find-spotted sensor movements time)]
    (add-sensed sensor time
                (-> observations
                   (insertion-noise sensor time)
                   (deletion-noise sensor time)
                   (distortion-noise sensor time)))))

(defn new-sensor
  "Generate a new sensor with provided values and an empty 'spotted' vector."
  [id left right bottom top sees-color]
  (init-sensor id sense {:left left :right right :bottom bottom :top top
                         :sees-color sees-color}))

(defn generate-sensors
  []
  (let [width-height (math/ceil
                      (math/sqrt (* (:GridHeight params)
                                    (:GridWidth params)
                                    (- 1.0 (* 0.01 (:SensorSeesColor params))))))
        left-right (math/ceil (/ (- (:GridWidth params) width-height) 2))
        top-bottom (math/ceil (/ (- (:GridHeight params) width-height) 2))]
    [(new-sensor (keyword "middle-gray")
                 left-right (- (:GridWidth params) left-right)
                 top-bottom (- (:GridHeight params) top-bottom)
                 false)
     (new-sensor (keyword "left")
                 0 (dec left-right) 0 (dec (:GridHeight params))
                 true)
     (new-sensor (keyword "right")
                 (inc (- (:GridWidth params) left-right))
                 (dec (:GridWidth params))
                 0
                 (dec (:GridHeight params))
                 true)
     (new-sensor (keyword "bottom")
                 left-right
                 (- (:GridWidth params) left-right)
                 (inc (- (:GridHeight params) top-bottom))
                 (dec (:GridHeight params))
                 true)
     (new-sensor (keyword "top")
                 left-right
                 (- (:GridWidth params) left-right)
                 0
                 (dec top-bottom)
                 true)]))

(comment [(new-sensor (keyword "1") 0 4 0 15 true)
          (new-sensor (keyword "2g") 0 4 16 20 false)
          (new-sensor (keyword "3") 0 4 21 29 true)
          (new-sensor (keyword "4g") 5 8 0 10 false)
          (new-sensor (keyword "5") 5 14 16 20 true)
          (new-sensor (keyword "5g") 5 10 21 24 false)
          (new-sensor (keyword "6") 15 29 0 20 true)
          (new-sensor (keyword "6g") 15 29 21 29 false)])


