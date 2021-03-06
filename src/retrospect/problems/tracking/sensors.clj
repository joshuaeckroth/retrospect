(ns retrospect.problems.tracking.sensors
  (:require [clojure.set :as set])
  (:use [retrospect.problems.tracking.colors])
  (:use [retrospect.sensors :only [init-sensor add-sensed]])
  (:use [retrospect.problems.tracking.movements :only
         [entity-movements entities-at]])
  (:use [geppetto.random])
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
                                (entity-movements movements e time)))
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
    (if (and (:Noise params)
             (< (my-rand) (/ (double (:SensorInsertionNoise params)) 100.0))
             (not-any? (fn [{x :x y :y}] (and (= (:x noise) x)
                                              (= (:y noise) y)))
                       observations))
      (conj observations noise)
      ;; no noise
      observations)))

(defn shuffle-observations
  [observations]
  (my-shuffle (sort-by (fn [ob] [(:x ob) (:y ob) (:time ob)]) observations)))

(defn deletion-noise
  [observations]
  (if (and (not-empty observations)
           (:Noise params)
           (< (my-rand) (/ (double (:SensorDeletionNoise params)) 100.0)))
    ;; remove an observation
    (rest (shuffle-observations observations))
    ;; otherwise, don't
    observations))

(defn distortion-noise
  [observations]
  ;; each observation has the form {:x # :y # :time # :color ""}
  (if (and (not-empty observations)
           (:Noise params)
           (< (my-rand) (/ (double (:SensorDistortionNoise params)) 100.0)))
    ;; change an observation
    (let [obs-shuffled (shuffle-observations observations)
          {:keys [x y time color]} (first obs-shuffled)
          ;; random number in range [-2,2]
          new-x (+ x (- (my-rand-int 5) 2))
          new-y (+ y (- (my-rand-int 5) 2))]
      (shuffle-observations (conj (rest obs-shuffled) {:x new-x :y new-y :time time :color color})))
    ;; otherwise, don't
    observations))

(defn duplication-noise
  [observations]
  ;; each observation has the form {:x # :y # :time # :color ""}
  (if (and (not-empty observations)
           (:Noise params)
           (< (my-rand) (/ (double (:SensorDuplicationNoise params)) 100.0)))
    ;; add an observation
    (let [obs-shuffled (shuffle-observations observations)
          {:keys [x y time color]} (first obs-shuffled)
          ;; random number in range [-2,2]
          new-x (+ x (- (my-rand-int 5) 2))
          new-y (+ y (- (my-rand-int 5) 2))]
      (shuffle-observations (conj obs-shuffled {:x new-x :y new-y :time time :color color})))
    ;; otherwise, don't
    observations))

(defn compute-virtual-scores
  [new-observations orig-observations]
  (let [orig-set (set orig-observations)]
    ;; keep the doall so the random numbers are generated the same across different runs
    (doall (for [obs new-observations]
             (if (not (:VirtualScores params)) obs
                 (let [r (my-rand)
                       good-bin? (or (and (orig-set obs) (< r (:VirtualScoresGoodProb params)))
                                     (and (not (orig-set obs)) (>= r (:VirtualScoresGoodProb params))))
                       p (if (= "gaussian" (:VirtualScoresMethod params))
                           (if good-bin?
                             (my-rand-gauss (:VirtualScoresGoodMean params)
                                            (:VirtualScoresGoodVariance params))
                             (my-rand-gauss (:VirtualScoresBadMean params)
                                            (:VirtualScoresBadVariance params)))
                           ;; else, "uniform" virtual scores
                           (if good-bin?
                             (- 1.0 (* (my-rand) (- 1.0 (:VirtualScoresGoodLowerBound params))))
                             (* (my-rand) (:VirtualScoresBadUpperBound params))))
                       apriori (min 1.0 (max p 0.0))]
                   (assoc obs :apriori apriori)))))))

(defn sense
  [sensor movements time]
  (let [observations (find-spotted sensor movements time)
        all-sensed-obs (doall (-> observations
                                  (insertion-noise sensor time)
                                  (distortion-noise)
                                  (duplication-noise)
                                  (deletion-noise)
                                  (compute-virtual-scores observations)))]
    (add-sensed sensor time all-sensed-obs)))

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
          top-bottom (int (Math/ceil (/ (- (:GridHeight params) width-height) 2)))
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


