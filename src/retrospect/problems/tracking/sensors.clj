(ns retrospect.problems.tracking.sensors
  (:use [retrospect.confidences])
  (:use [retrospect.random])
  (:use [retrospect.colors :only [red blue green gray]])
  (:use [retrospect.problems.tracking.grid :only [grid-entities]])
  (:use [retrospect.sensors :only [init-sensor add-sensed]])
  (:require [clojure.contrib.math :as math]))

(defn sees
  [sensor x y]
  (and (>= x (:left (meta sensor))) (<= x (:right (meta sensor)))
       (>= y (:bottom (meta sensor))) (<= y (:top (meta sensor)))))

(defn sensors-seen-grid
  [sensors params]
  (with-meta
    (for [x (range (:GridWidth params)) y (range (:GridHeight params))]
      (filter (fn [s] (sees s x y)) sensors))
    {:width (:GridWidth params) :height (:GridHeight params)}))

(defn list-sensors-seen
  [width height sensors]
  (doall (filter identity
                 (for [x (range width) y (range height)]
                   (if (some #(sees % x y) sensors) {:x x :y y})))))

(defn list-sensors-unseen
  [width height sensors]
  (let [seen (list-sensors-seen width height sensors)]
    (doall (filter identity
                   (for [x (range width) y (range height)]
                     (let [p {:x x :y y}]
                       (if-not (some #(= % p) seen) p)))))))

(defn find-spotted
  [sensor grid time]
  (filter (fn [e] (sees sensor (:x (meta e)) (:y (meta e))))
          (grid-entities grid)))

(defn adjust-ids
  [es sees-color?]
  (if sees-color?
    (concat (map (fn [e] (with-meta (symbol (str "B")) (meta e)))
                 (filter #(= blue (:color (meta %))) es))
            (map (fn [e] (with-meta (symbol (str "R")) (meta e)))
                 (filter #(= red (:color (meta %))) es))
            (map (fn [e] (with-meta (symbol (str "E")) (meta e)))
                 (filter #(= green (:color (meta %))) es)))
    (map (fn [e] (with-meta (symbol (str "G")) (assoc (meta e) :color gray))) es)))

(defn sense
  [sensor grid time]
  (let [spotted (find-spotted sensor grid time)
        color-adjusted (if (:sees-color (meta sensor))
                         (adjust-ids spotted true)
                         (adjust-ids spotted false))
        sensed (vals (reduce (fn [m e] (assoc m (meta e) e)) {} color-adjusted))]
    (add-sensed sensor time (if sensed sensed []))))

(defn new-sensor
  "Generate a new sensor with provided values and an empty 'spotted' vector."
  [id left right bottom top sees-color]
  (init-sensor id sense {:left left :right right :bottom bottom :top top
                         :sees-color sees-color}))

(defn generate-sensors
  [params]
  (let [num-sensors (inc (my-rand-int (:GridWidth params)))
        width-each (math/ceil (/ (:GridWidth params) num-sensors))]
    (for [i (range num-sensors)]
      (let [gray? (< (my-rand) (/ (:SensorSeesColor params) 100))]
        (new-sensor (keyword (format "%d%s" (inc i) (if gray? "g" "")))
                    (* i width-each) (dec (* (inc i) width-each))
                    0 (:GridHeight params) gray?)))))

(comment [(new-sensor (keyword "1") 0 4 0 15 true)
          (new-sensor (keyword "2g") 0 4 16 20 false)
          (new-sensor (keyword "3") 0 4 21 29 true)
          (new-sensor (keyword "4g") 5 8 0 10 false)
          (new-sensor (keyword "5") 5 14 16 20 true)
          (new-sensor (keyword "5g") 5 10 21 24 false)
          (new-sensor (keyword "6") 15 29 0 20 true)
          (new-sensor (keyword "6g") 15 29 21 29 false)])


