(ns retrospect.problems.tracking.movements
  (:use [geppetto.random])
  (:use [retrospect.problems.tracking.colors])
  (:use [retrospect.state]))

;; Entity movements are stored in a map, whose keys are entity
;; symbols. An entity symbol has metadata key :color. Also, the
;; movements map has metadata keys :width, :height.
;;
;; For each entity, its movements are a vector of maps, where each
;; map, except the first in the vector, has the keys :x, :y, :time,
;; :ox, :oy, :ot; the first map has the keys :x, :y, :time.

(defn new-movements
  [width height]
  (with-meta {} {:width width :height height}))

(defn move-entity
  [movements entity x y time]
  (let [last-pos (last (get movements entity))]
    (update-in movements [entity] conj
               {:ox (:x last-pos) :oy (:y last-pos) :ot (:time last-pos)
                :x x :y y :time time :color (:color last-pos)})))

(defn entities-at
  [movements x y time]
  (filter (fn [e] (and (< time (count (get movements e)))
                 (let [mov (nth (get movements e) time)]
                   (and (= x (:x mov)) (= y (:y mov))))))
     (keys movements)))

(defn new-entity
  [movements time]
  (let [[x y] [(my-rand-int (:width (meta movements)))
               (my-rand-int (:height (meta movements)))]
        c (rand-color)
        e (count (keys movements))]
    (if (not-empty (entities-at movements x y time))
      (recur movements time)
      (assoc movements e [{:x x :y y :time time :color c}]))))

(defn entity-movements
  ([movements entity time]
     [(nth (get movements entity) time)])
  ([movements entity mintime maxtime]
     (filter #(and (>= (:time %) mintime) (<= (:time %) maxtime))
        (get movements entity))))

(defn entities
  [movements]
  (keys movements))

(defn moves-match?
  [mov1 mov2]
  (and (= (:x mov1) (:x mov2))
       (= (:ox mov1) (:ox mov2))
       (= (:y mov1) (:y mov2))
       (= (:oy mov1) (:oy mov2))
       (= (:time mov1) (:time mov2))
       (= (:ot mov1) (:ot mov2))
       (match-color? (:color mov1) (:color mov2))))

(defn dets-match?
  [det det2]
  (and (= (:x det) (:x det2))
       (= (:y det) (:y det2))
       (= (:time det) (:time det2))
       (match-color? (:color det) (:color det2))))

(def dist
  (memoize
   (fn [x1 y1 x2 y2]
     (if (= "gaussian" (:WalkType params))
       (double (Math/sqrt (+ (* (- x1 x2) (- x1 x2))
                             (* (- y1 y2) (- y1 y2)))))
       ;; else, :WalkType = "random"
       ;; use manhattan distance
       (double (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))))))

(def loc-distances
  (memoize
   (fn [x y width height]
     (for [nx (range width) ny (range height)]
       [[nx ny] (dist x y nx ny)]))))

(defn constrain
  [x low high]
  (min (max x low) high))

(defn walk-random
  [walk-steps random? movements entity time]
  (let [width (:width (meta movements))
        height (:height (meta movements))
        movs (reverse (get movements entity))
        last-pos (first movs)
        [ox oy] [(:x last-pos) (:y last-pos)]]
    (loop [attempts 0]
      (let [[x y] [(constrain (+ ox (my-rand-nth (range (- walk-steps) (inc walk-steps)))) 0 (dec width))
                   (constrain (+ oy (my-rand-nth (range (- walk-steps) (inc walk-steps)))) 0 (dec height))]]
        (cond (= attempts 50)
              (move-entity movements entity ox oy time)
              (and x y
                   (empty? (entities-at movements x y time))
                   (empty? (entities-at movements x y (dec time))))
              (move-entity movements entity x y time)
              :else (recur (inc attempts)))))))

(defn generate-movements
  [movements time-steps walk-steps random?]
  (loop [time 1
         m movements]
    (if (> time time-steps)
      (if (:SequentialSensorReports params) m
          (with-meta (reduce (fn [m [e movs]] (assoc m e (my-shuffle movs))) {} (seq m))
            (meta m)))
      (recur (inc time) (reduce #(walk-random walk-steps random? %1 %2 time)
                           m (my-shuffle (entities m)))))))
