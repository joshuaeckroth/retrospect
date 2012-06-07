(ns retrospect.reason.abduction.problems.tracking.hypotheses
  (:require [clojure.string :as str])
  (:use [clojure.contrib.seq :only [find-first]])
  (:use [retrospect.reason.abduction.workspace :only [new-hyp]])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.problems.tracking.colors])
  (:use [retrospect.problems.tracking.movements
         :only [dist dets-match?]])
  (:use [retrospect.reason.abduction.problems.tracking.pathsgraph
         :only [get-paths build-paths-graph path-str]])
  (:use [retrospect.profile :only [prof]])
  (:use [retrospect.state]))

(defn generate-kb
  [training]
  [(new-hyp "KB" :kb :kb false (constantly false) [] [] "" "" {})])

(defn make-sensor-hyps
  [sensor time-prev time-now hyps]
  (prof :make-sensor-hyps
        (mapcat (fn [{:keys [x y color time] :as det}]
                  (let [desc (format (str "Sensor detection by %s - color: %s, "
                                     "x: %d, y: %d, time: %d")
                                (:id sensor) (color-str color) x y time)
                        from (new-hyp "SensFrom" :sensor :sensor-from
                                      true (constantly false) [] []
                                      (format "%d,%d@%d" x y time) desc
                                      {:sensor sensor :det det})
                        to (new-hyp "SensTo" :sensor :sensor-to
                                    true (constantly false) [] []
                                    (format "%d,%d@%d" x y time) desc
                                    {:sensor sensor :det det})]
                    (cond (= time time-prev) [to]
                          (= time time-now) [from]
                          :else [from to])))
                (mapcat #(sensed-at sensor %) (range time-prev (inc time-now))))))

(defn conflicts?
  [h1 h2]
  (and (not= (:id h1) (:id h2))
       (= :movement (:type h1) (:type h2))
       (or (and (= (:x (:det h1)) (:x (:det h2)))
                (= (:time (:det h1)) (:time (:det h2))))
           (and (= (:x (:det h1)) (:x (:det2 h2)))
                (= (:time (:det h1)) (:time (:det2 h2))))
           (and (= (:x (:det h1)) (:x (:det2 h2)))
                (= (:time (:det h1)) (:time (:det2 h2))))
           (and (= (:x (:det2 h1)) (:x (:det2 h2)))
                (= (:time (:det2 h1)) (:time (:det2 h2)))))
       (or (and (= (:y (:det h1)) (:y (:det h2)))
                (= (:time (:det h1)) (:time (:det h2))))
           (and (= (:y (:det h1)) (:y (:det2 h2)))
                (= (:time (:det h1)) (:time (:det2 h2))))
           (and (= (:y (:det h1)) (:y (:det2 h2)))
                (= (:time (:det h1)) (:time (:det2 h2))))
           (and (= (:y (:det2 h1)) (:y (:det2 h2)))
                (= (:time (:det2 h1)) (:time (:det2 h2)))))))

(defn connecting-movs
  [h acc-mov-hyps]
  (letfn [(match-loc [det det2] (and (= (:x det) (:x det2))
                                     (= (:y det) (:y det2))
                                     (= (:time det) (:time det2))))]
    (filter (fn [h2] (or (match-loc (:det h) (:det2 h2))
                   (match-loc (:det2 h) (:det h2))))
       acc-mov-hyps)))

(defn filter-valid-movs
  [mov-hyps acc-mov-hyps]
  (prof :filter-valid-movs
        (letfn [(valid? ([h] (let [c (connecting-movs h acc-mov-hyps)]
                               (or (empty? c)
                                   (some #(dets-match? (:det h) (:det2 %)) c)
                                   (some #(dets-match? (:det %) (:det2 h)) c)))))]
          (filter valid? mov-hyps))))

(defn new-mov-hyp
  [to from det-color det2-color]
  (new-hyp "Mov" :movement
           [(:x det-color) (:y det-color) (:x det2-color) (:y det2-color)]
           false conflicts? [to from] []
           (path-str [det-color det2-color])
           (format "%s (dist=%.2f)"
                   (path-str [det-color det2-color])
                   (dist (:x det-color) (:y det-color)
                         (:x det2-color) (:y det2-color)))
           {:det det-color :det2 det2-color
            :mov {:x (:x det2-color) :y (:y det2-color) :time (:time det2-color)
                  :ox (:x det-color) :oy (:y det-color) :ot (:time det-color)
                  :color (:color det-color)}}))

(defn new-mov-hyps
  [to from acc-mov-hyps]
  (prof :new-mov-hyps
        (let [det (:det to) det2 (:det from)
              colors-in (set (map (comp :color :det2)
                                (connecting-movs {:det det :det2 det2} acc-mov-hyps)))
              colors-out (set (map (comp :color :det)
                                 (connecting-movs {:det det :det2 det2} acc-mov-hyps)))
              det-color (cond (and (= gray (:color det))
                                   (not= gray (:color det2)))
                              (assoc det :color (:color det2))
                              (and (= gray (:color det))
                                   (= 1 (count colors-in)))
                              (assoc det :color (first colors-in))
                              :else det)
              det2-color (cond (and (= gray (:color det2))
                                    (not= gray (:color det)))
                               (assoc det2 :color (:color det))
                               (and (= gray (:color det2))
                                    (= 1 (count colors-in)))
                               (assoc det2 :color (first colors-in))
                               :else det2)]
          (if (= gray (:color det-color) (:color det2-color))
            (map #(new-mov-hyp to from
                             (assoc det-color :color %)
                             (assoc det2-color :color %))
               [red green blue])
            [(new-mov-hyp to from det-color det2-color)]))))

(defn movement-in-range?
  [to from]
  (let [{x1 :x y1 :y} (:det to)
        {x2 :x y2 :y} (:det from)
        d (dist x1 y1 x2 y2)]
    (<= d (* (Math/sqrt 2) (:MaxWalk params)))))

(defn hypothesize
  [sensor-hyps accepted lookup-hyp]
  (prof :hypothesize
        (let [from-hyps (filter #(= :sensor-from (:subtype %)) sensor-hyps)
              to-hyps (filter #(= :sensor-to (:subtype %)) sensor-hyps)]
          (mapcat
           (fn [evidence]
             (let [acc-mov-hyps (map lookup-hyp (get accepted :movement))
                   nearby (filter #(movement-in-range? evidence %) to-hyps)
                   mov-hyps (mapcat #(new-mov-hyps % evidence acc-mov-hyps) nearby)]
               (filter-valid-movs mov-hyps acc-mov-hyps)))
           from-hyps))))

(defn update-kb
  [accepted unexplained hypotheses lookup-hyp]
  (map lookup-hyp (get accepted :kb)))

