(ns retrospect.problems.tracking.sensors
  (:use [retrospect.confidences])
  (:use [retrospect.random])
  (:use [retrospect.colors])
  (:use [retrospect.sensors :only [init-sensor add-sensed]])
  (:require [clojure.contrib.math :as math])
  (:use [retrospect.problems.tracking.movements :only
         [entity-movements entities-at]])
  (:use [retrospect.state]))

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

(defn sense
  [sensor movements time]
  (add-sensed sensor time (find-spotted sensor movements time)))

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


