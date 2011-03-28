(ns retrospect.problems.tracking.sensors
  (:use [retrospect.confidences])
  (:use [retrospect.colors :only [gray]])
  (:use [retrospect.problems.tracking.grid :only [grid-entities]])
  (:use [retrospect.sensors :only [init-sensor add-sensed]]))

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

(defn sense
  [sensor grid time]
  (let [spotted (find-spotted sensor grid time)
        id-adjusted (map (fn [e] (with-meta (symbol (str "X")) (meta e))) spotted)
        color-adjusted (if (:sees-color (meta sensor)) id-adjusted
                           (map (fn [e] (with-meta e (assoc (meta e) :color gray)))
                                id-adjusted))]
    (add-sensed sensor time
                (vals (reduce (fn [m e] (assoc m (meta e) e)) {} color-adjusted)))))

(defn new-sensor
  "Generate a new sensor with provided values and an empty 'spotted' vector."
  [id left right bottom top sees-color]
  (init-sensor id sense {:left left :right right :bottom bottom :top top
                         :sees-color sees-color}))

(defn measure-sensor-coverage
  [width height sensors]
  (let [markers (for [x (range width) y (range height)]
		  (if (some #(sees % x y) sensors) 1 0))
	covered (reduce + markers)]
    (double (* 100 (/ covered (* width height))))))

(defn measure-sensor-overlap
  [width height sensors]
  (let [count-xy
	(for [x (range width) y (range height)]
          (count (filter identity (map (fn [s] (sees s x y)) sensors))))]
    (double (/ (reduce + count-xy) (* width height)))))

(defn inside?
  [s1 s2]
  (and
   (not= (:id s1) (:id s2))
   (>= (:left (meta s1)) (:left (meta s2)))
   (<= (:right (meta s1)) (:right (meta s2)))
   (>= (:bottom (meta s1)) (:bottom (meta s2)))
   (<= (:top (meta s1)) (:top (meta s2)))))

(defn sensor-inside-another?
  [sensor sensors]
  (some #(inside? sensor %) sensors))

(defn generate-sensors-sample
  [width height sees-color-prob]
  (doall (for [i (range (rand-int (* width height)))]
           (let [left (rand-int width)
                 right (+ left (rand-int (- width left)))
                 bottom (rand-int height)
                 top (+ bottom (rand-int (- height bottom)))]
             (new-sensor (keyword (format "Sensor%d" (hash (rand))))
                         left right bottom top
                         (> (double (/ sees-color-prob 100)) (rand)))))))

(defn generate-sensors-with-coverage
  [width height coverage sees-color-prob]
  (loop [sensors (generate-sensors-sample width height sees-color-prob)]
    (let [measured (measure-sensor-coverage width height sensors)]
      (if (and
           (> 4.0 (measure-sensor-overlap width height sensors))
           (> measured (- coverage 5.0)) (< measured (+ coverage 5.0)))
        (filter #(not (sensor-inside-another? % sensors)) sensors)
        (recur (generate-sensors-sample width height sees-color-prob))))))

(defn generate-sensors
  [params]
  [(new-sensor (keyword "1") 0 4 0 29 true)
   (new-sensor (keyword "1g") 5 9 0 29 false)
   (new-sensor (keyword "2") 10 14 0 29 true)
   (new-sensor (keyword "2g") 15 19 0 29 false)
   (new-sensor (keyword "3") 20 24 0 29 true)
   (new-sensor (keyword "3g") 25 29 0 29 false)])

(comment [(new-sensor (keyword "1") 0 4 0 15 true)
          (new-sensor (keyword "2g") 0 4 16 20 false)
          (new-sensor (keyword "3") 0 4 21 29 true)
          (new-sensor (keyword "4g") 5 8 0 10 false)
          (new-sensor (keyword "5") 5 14 16 20 true)
          (new-sensor (keyword "5g") 5 10 21 24 false)
          (new-sensor (keyword "6") 15 29 0 20 true)
          (new-sensor (keyword "6g") 15 29 21 29 false)])

(comment (defn generate-sensors
           [params]
           (generate-sensors-with-coverage
             (:GridWidth params) (:GridHeight params)
             (:SensorCoverage params) (:SensorSeesColor params))))

