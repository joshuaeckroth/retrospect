(ns retrospect.problems.tracking.sensors
  (:use [retrospect.random])
  (:use [retrospect.problems.tracking.colors])
  (:use [retrospect.sensors :only [init-sensor add-sensed]])
  (:require [clojure.contrib.math :as math])
  (:use [retrospect.problems.tracking.movements :only
         [entity-movements entities-at]])
  (:use [retrospect.random])
  (:use [retrospect.state]))

(defn perturb
  [sensor]
  sensor)

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
  (let [x (my-rand-nth (range (:left (meta sensor)) (inc (:right (meta sensor)))))
        y (my-rand-nth (range (:bottom (meta sensor)) (inc (:top (meta sensor)))))
        color (if (:sees-color (meta sensor)) (my-rand-nth (:colors (meta sensor))) gray)]
    {:x x :y y :time time :color color}))

(defn insertion-noise
  [observations sensor time]
  ;; each observation has the form {:x # :y # :time # :color (object)}
  (let [noise (make-random-det sensor time)]
    (if (and (< (my-rand) (/ (double (:SensorInsertionNoise params)) 100.0))
             (not-any? (fn [{x :x y :y}] (and (= (:x noise) x)
                                             (= (:y noise) y)))
                       observations))
      (conj observations noise)
      ;; no noise
      observations)))

(defn deletion-noise
  [observations]
  (if (and (not-empty observations)
           (:Noise params)
           (< (my-rand) (/ (double (:SensorDeletionNoise params)) 100.0)))
    ;; remove an observation
    (rest (my-shuffle (sort-by vec observations)))
    ;; otherwise, don't
    observations))

(defn distortion-noise
  [observations]
  ;; each observation has the form {:x # :y # :time # :color ""}
  (if (and (not-empty observations)
           (:Noise params)
           (< (my-rand) (/ (double (:SensorDistortionNoise params)) 100.0)))
    ;; change an observation
    (let [obs-shuffled (my-shuffle (sort-by vec observations))
          {:keys [x y time color]} (first obs-shuffled)
          ;; random number in range [-2,2]
          new-x (+ x (- (my-rand-int 5) 2))
          new-y (+ y (- (my-rand-int 5) 2))]
      (my-shuffle (conj (rest obs-shuffled) {:x new-x :y new-y :time time :color color})))
    ;; otherwise, don't
    observations))

(defn duplication-noise
  [observations]
  ;; each observation has the form {:x # :y # :time # :color ""}
  (if (and (not-empty observations)
           (:Noise params)
           (< (my-rand) (/ (double (:SensorDuplicationNoise params)) 100.0)))
    ;; add an observation
    (let [obs-shuffled (my-shuffle (sort-by vec observations))
          {:keys [x y time color]} (first obs-shuffled)
          ;; random number in range [-2,2]
          new-x (+ x (- (my-rand-int 5) 2))
          new-y (+ y (- (my-rand-int 5) 2))]
      (my-shuffle (conj obs-shuffled {:x new-x :y new-y :time time :color color})))
    ;; otherwise, don't
    observations))

(defn sense
  [sensor movements time]
  (let [observations (find-spotted sensor movements time)]
    (add-sensed sensor time
                (-> observations
                   (insertion-noise sensor time)
                   (deletion-noise)
                   (distortion-noise)))))

(defn new-sensor
  "Generate a new sensor with provided values and an empty 'spotted' vector."
  [id left right bottom top sees-color colors]
  (init-sensor id sense {:left left :right right :bottom bottom :top top
                         :sees-color sees-color :colors (sort-by str colors)}))

(defn generate-sensors
  [training]
  (if (= 100 (:SensorSeesColor params))
    [(new-sensor (keyword "main-sensor")
                 0 (dec (:GridWidth params))
                 0 (dec (:GridHeight params))
                 true
                 (:seen-colors training))]
    (let [width-height (* (:GridHeight params)
                          (- 1.0 (* 0.01 (:SensorSeesColor params))))
          top-bottom (int (math/ceil (/ (- (:GridHeight params) width-height) 2)))
          colors (:seen-colors training)]
      (filter identity
         [(new-sensor (keyword "middle-gray")
                      0 (dec (:GridWidth params))
                      top-bottom (max 0 (min (dec (:GridHeight params)) (- (:GridHeight params) top-bottom)))
                      false
                      colors)
          (when (> top-bottom 1)
            (new-sensor (keyword "bottom")
                        0 (dec (:GridWidth params))
                        (max 0 (min (dec (:GridWidth params)) (inc (- (:GridHeight params) top-bottom))))
                        (dec (:GridHeight params))
                        true
                        colors))
          (when (> top-bottom 0)
            (new-sensor (keyword "top")
                        0 (dec (:GridWidth params))
                        0 (max 0 (min (dec (:GridHeight params)) (dec top-bottom)))
                        true
                        colors))]))))

(comment [(new-sensor (keyword "1") 0 4 0 15 true)
          (new-sensor (keyword "2g") 0 4 16 20 false)
          (new-sensor (keyword "3") 0 4 21 29 true)
          (new-sensor (keyword "4g") 5 8 0 10 false)
          (new-sensor (keyword "5") 5 14 16 20 true)
          (new-sensor (keyword "5g") 5 10 21 24 false)
          (new-sensor (keyword "6") 15 29 0 20 true)
          (new-sensor (keyword "6g") 15 29 21 29 false)])


