(ns retrospect.reason.abduction.problems.tracking.hypotheses
  (:require [clojure.string :as str])
  (:use [clojure.contrib.seq :only [find-first]])
  (:use [retrospect.reason.abduction.workspace :only [new-hyp]])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.problems.tracking.colors])
  (:use [retrospect.problems.tracking.movements
         :only [dist dets-match?]])
  (:use [retrospect.profile :only [prof]])
  (:use [retrospect.random])
  (:use [retrospect.state]))

(defn compute-moves-dist
  [moves]
  (let [dists (map #(dist (:ox %) (:oy %) (:x %) (:y %)) moves)
        mean (/ (reduce + dists) (count dists))
        variance (/ (reduce + (map #(Math/pow (- mean %) 2.0) dists)) (count dists))]
    {:mean mean :variance variance}))

(defn generate-kb
  [training]
  (let []
    [(new-hyp "KB" :kb :kb 1.0 false nil [] "" ""
              {:moves (:moves training)
               :moves-dist (compute-moves-dist (:moves training))
               :seen-colors (:seen-colors training)})]))

(defn get-kb
  [accepted lookup-hyp]
  (first (filter #(= :kb (:subtype %)) (map lookup-hyp (get accepted :kb)))))

(defn update-kb
  [accepted unexplained hypotheses lookup-hyp]
  (let [mov-hyps (map lookup-hyp (get accepted :movement))
        old-kb (get-kb accepted lookup-hyp)
        kb-moves (update-in old-kb [:moves] concat (map :mov mov-hyps))
        kb-moves-dist (assoc kb-moves :moves-dist (compute-moves-dist (:moves kb-moves)))]
    [kb-moves-dist]))

(defn conflicts?
  [h1 h2]
  (and (not= (:id h1) (:id h2))
       (or
        (and (= :ignore (:subtype h1))
             (= :observation (:type h2))
             (= (:det h1) (:det h2))
             (= (:from-to h1) (:from-to h2)))
        (and (= :ignore (:subtype h2))
             (= :observation (:type h1))
             (= (:det h2) (:det h1))
             (= (:from-to h1) (:from-to h2)))
        (and
         (= :movement (:type h1) (:type h2))
         (let [mov1 (:mov h1)
               mov2 (:mov h2)]
           (or
            ;; start at same place
            (and (= (:x mov1) (:x mov2))
                 (= (:y mov1) (:y mov2))
                 (= (:time mov1) (:time mov2)))
            ;; end at same place
            (and (= (:ox mov1) (:ox mov2))
                 (= (:oy mov1) (:oy mov2))
                 (= (:ot mov1) (:ot mov2)))
            ;; mov1 ends where mov2 starts and not same color
            (and (= (:x mov1) (:ox mov2))
                 (= (:y mov1) (:oy mov2))
                 (= (:time mov1) (:ot mov2))
                 (not (match-color? (:color mov1) (:color mov2))))
            ;; mov2 ends where mov1 starts and not same color
            (and (= (:x mov2) (:ox mov1))
                 (= (:y mov2) (:oy mov1))
                 (= (:time mov2) (:ot mov1))
                 (not (match-color? (:color mov2) (:color mov1))))
            ;; same color (use =, not match-color?), same time,
            ;; different paths
            (and (= (:color mov1) (:color mov2))
                 (or (= (:time mov1) (:time mov2))
                     (= (:ot mov1) (:ot mov2))))))))))

(defn make-sensor-hyps
  [sensors time-prev time-now accepted lookup-hyp]
  (prof :make-sensor-hyps
        (doall
         (mapcat (fn [{:keys [x y color time] :as det}]
                   (let [desc (format (str "Sensor detection - color: %s, "
                                      "x: %d, y: %d, time: %d")
                                 (color-str color) x y time)
                         from (new-hyp "SensFrom" :observation :from
                                       1.0 true conflicts? []
                                       (format "%d,%d@%d" x y time) desc
                                       {:det det :from-to :from})
                         to (new-hyp "SensTo" :observation :to
                                     1.0 true conflicts? []
                                     (format "%d,%d@%d" x y time) desc
                                     {:det det :from-to :to})]
                     (cond (= time time-prev) [to]
                           (= time time-now) [from]
                           :else [from to])))
                 (sort-by :time (mapcat (fn [t] (mapcat (fn [s] (sensed-at s t)) sensors))
                                        (range time-prev (inc time-now))))))))

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

(defn move-prob
  [dist moves-dist]
  (cumprob (:mean moves-dist) (:variance moves-dist) (- dist 2.0) (+ dist 2.0)))

(defn new-mov-hyp
  "Returns nil if the colors don't match."
  [to from acc-mov-hyps moves-dist seen-colors]
  (prof :new-mov-hyp
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
                               :else det2)
              d (dist (:x det-color) (:y det-color)
                      (:x det2-color) (:y det2-color))]
          (when (match-color? (:color det-color) (:color det2-color))
            (new-hyp "Mov" :movement :movement
                     (move-prob d moves-dist)
                     false conflicts? (map :contents [to from])
                     (format "%d,%d->%d,%d @ %d->%d (%s->%s)"
                        (:x det-color) (:y det-color)
                        (:x det2-color) (:y det2-color)
                        (:time det-color) (:time det2-color)
                        (color-str (:color det-color))
                        (color-str (:color det2-color)))
                     (format "%d,%d -> %d,%d (dist=%.2f) at time %d->%d (%s->%s)"
                        (:x det-color) (:y det-color)
                        (:x det2-color) (:y det2-color)
                        d (:time det-color) (:time det2-color)
                        (color-str (:color det-color))
                        (color-str (:color det2-color)))
                     {:det det-color :det2 det2-color
                      :mov {:x (:x det2-color) :y (:y det2-color) :time (:time det2-color)
                            :ox (:x det-color) :oy (:y det-color) :ot (:time det-color)
                            :color (:color det-color)}})))))

(defn dets-connected?
  [to from]
  (let [{x1 :x y1 :y} (:det to)
        {x2 :x y2 :y} (:det from)
        d (dist x1 y1 x2 y2)]
    (and (<= d (:MaxWalk params))
         (= (:time (:det to)) (inc (:time (:det from)))))))

(defn hypothesize
  [sensor-hyps accepted lookup-hyp time-now]
  (prof :hypothesize
        (let [from-hyps (filter #(= :from (:subtype %))
                           (sort-by (comp :time :det) sensor-hyps))
              to-hyps (filter #(= :to (:subtype %))
                         (sort-by (comp :time :det) sensor-hyps))
              kb (get-kb accepted lookup-hyp)]
          (doall (mapcat
                  (fn [evidence]
                    (let [acc-mov-hyps (sort-by (comp :time :mov)
                                                (map lookup-hyp (get accepted :movement)))
                          nearby (filter #(dets-connected? evidence %) to-hyps)
                          mov-hyps (doall
                                    (filter identity
                                       (map #(new-mov-hyp % evidence acc-mov-hyps
                                                        (:moves-dist kb) (:seen-colors kb))
                                          nearby)))]
                      (filter-valid-movs mov-hyps acc-mov-hyps)))
                  from-hyps)))))
