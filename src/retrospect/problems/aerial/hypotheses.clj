(ns retrospect.problems.aerial.hypotheses
  (:require [clojure.set :as set])
  (:use [retrospect.reason.abduction.workspace :only [new-hyp]])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.evaluate :only [avg]])
  (:use [retrospect.state])
  (:use [geppetto.profile :only [prof]]))

(defn dist
  [x1 y1 x2 y2]
  (double (Math/sqrt (+ (* (- x1 x2) (- x1 x2))
                        (* (- y1 y2) (- y1 y2))))))

(defn compute-moves-dist
  [moves]
  0.5)

(defn generate-kb
  [training]
  [(new-hyp "KB" :kb :kb 1.0 false nil nil [] "" ""
            {:moves (:moves training)
             :moves-dist (compute-moves-dist (:moves training))})])

(defn get-kb
  [accepted]
  (first (get accepted :kb)))

;; TODO
(defn update-kb
  [accepted unexplained hypotheses]
  (let [mov-hyps (:movement accepted)
        old-kb (get-kb accepted)
        kb-moves (update-in old-kb [:moves] concat (map :mov mov-hyps))
        kb-moves-dist (assoc kb-moves :moves-dist (compute-moves-dist (:moves kb-moves)))]
    [kb-moves-dist]))

(defn move-prob
  [det det2 moves-dist]
  (/ (+ (:detscore det) (:detscore det2)) 2.0))

(defn conflicts?
  [h1 h2]
  (cond (= :movement (:type h1))
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
            ;; mov1 ends where mov2 starts and not same objid
            (and (= (:x mov1) (:ox mov2))
                 (= (:y mov1) (:oy mov2))
                 (= (:time mov1) (:ot mov2))
                 (not= (:objid mov1) (:objid mov2)))
            ;; mov2 ends where mov1 starts and not same objid
            (and (= (:x mov2) (:ox mov1))
                 (= (:y mov2) (:oy mov1))
                 (= (:time mov2) (:ot mov1))
                 (not= (:objid mov1) (:objid mov2)))
            ;; same objid, same start-end times but not start-end locations
            (and (= (:objid mov1) (:objid mov2))
                 (or (and (= (:time mov1) (:ot mov2))
                          (or (not= (:x mov1) (:ox mov2))
                              (not= (:y mov1) (:oy mov2))))
                     (and (= (:ot mov1) (:time mov2))
                          (or (not= (:ox mov1) (:x mov2))
                              (not= (:oy mov1) (:y mov2))))))
            ;; same objid, same time, different paths
            (and (= (:objid mov1) (:objid mov2))
                 (or (= (:time mov1) (:time mov2))
                     (= (:ot mov1) (:ot mov2)))))))))

(defn make-sensor-hyp
  [{:keys [x y time detscore] :as det} from-to other-dets moves-dist]
  (new-hyp (format "Sens%s" (if (= :from from-to) "From" "To"))
           :observation from-to (:detscore det)
           true nil nil []
           (format "%.2f, %.2f @ %d" x y time)
           (format "Sensor detection - x: %.2f, y: %.2f, time: %d, detscore: %.2f" x y time detscore)
           {:det det :from-to from-to}))

(defn make-sensor-hyps
  [sensors time-prev time-now accepted hypotheses]
  (let [kb (get-kb accepted)
        moves-dist (:moves-dist kb)
        acc-dets (map :det (:observation accepted))
        sensed-dets (mapcat (fn [t] (sensed-at (first sensors) t))
                            (range time-prev (inc time-now)))
        prior-next-dets (concat acc-dets sensed-dets)
        prior-dets (filter #(= (dec time-now) (:time %)) prior-next-dets)
        next-dets (filter #(= time-now (:time %)) prior-next-dets)
        to-time (if (:SequentialSensorReports params) time-prev 0)
        from-time (if (:SequentialSensorReports params) time-now (:Steps params))]
    (doall
     (if (= time-prev time-now) []
         (mapcat (fn [det] (cond
                            ;; if det has time 0 or time-prev, only generate "to" report
                            (and (= (:time det) to-time))
                            [(make-sensor-hyp det :to next-dets moves-dist)]
                            ;; if det has time equal to steps or time-now,
                            ;; only generate "from" report
                            (and (= (:time det) from-time))
                            [(make-sensor-hyp det :from prior-dets moves-dist)]
                            ;; otherwise, generate both "from" and "to" reports
                            (and (not= (:time det) to-time)
                                 (not= (:time det) from-time))
                            [(make-sensor-hyp det :to next-dets moves-dist)
                             (make-sensor-hyp det :from prior-dets moves-dist)]
                            :else []))
                 (sort-by :time sensed-dets))))))

(defn random-objid
  []
  (str (java.util.UUID/randomUUID)))

(defn dets-match?
  [det det2]
  (and (= (:x det) (:x det2))
       (= (:y det) (:y det2))
       (= (:time det) (:time det2))))

(defn new-mov-hyp
  [to from acc-mov-hyps moves-dist]
  (prof :new-mov-hyp
        (let [det (:det to) det2 (:det from)
              d (dist (:x det) (:y det) (:x det2) (:y det2))
              apriori (move-prob det det2 moves-dist)
              ;; these should all be the same, if not empty
              objids (map (comp :objid :mov)
                          (filter (fn [{mdet :det mdet2 :det2}]
                                    (or (dets-match? mdet det2)
                                        (dets-match? mdet2 det)))
                                  acc-mov-hyps))
              objid (if (empty? objids) (random-objid) (first objids))]
          (new-hyp "Mov" :movement :movement apriori false
                   [det det2 objid]
                   conflicts? (map :contents [to from])
                   (format "%.2f, %.2f -> %.2f, %.2f @ %d->%d"
                           (:x det) (:y det)
                           (:x det2) (:y det2)
                           (:time det) (:time det2))
                   (format "%.2f, %.2f -> %.2f, %.2f (dist=%.2f) at time %d->%d\nDetscores: %.2f -> %.2f\nID: %s"
                           (:x det) (:y det)
                           (:x det2) (:y det2)
                           d (:time det) (:time det2)
                           (:detscore det) (:detscore det2)
                           objid)
                   {:det det :det2 det2
                    :mov {:x (:x det2) :y (:y det2) :time (:time det2)
                          :ox (:x det) :oy (:y det) :ot (:time det)
                          :objid objid :dist d}}))))

(defn dets-nearby?
  [to from]
  (let [det (:det to)
        det2 (:det from)
        d (dist (:x det2) (:y det2) (:x det) (:y det))]
    (and (< d 10.0)
         (= (:time (:det to)) (inc (:time (:det from)))))))

(defn hypothesize
  [unexp accepted hypotheses time-now]
  (prof :hypothesize
        (let [kb (get-kb accepted)
              sensor-from-hyps (filter #(and (= :observation (:type %)) (= :from (:subtype %))) unexp)
              sensor-to-hyps (filter #(and (= :observation (:type %)) (= :to (:subtype %))) unexp)]
          (doall (mapcat
                  (fn [evidence]
                    (let [acc-mov-hyps (sort-by (comp :time :mov) (:movement accepted))
                          nearby (filter #(dets-nearby? evidence %) sensor-to-hyps)
                          mov-hyps (doall (map #(new-mov-hyp % evidence acc-mov-hyps (:moves-dist kb))
                                               nearby))]
                      (filter #(< 0.01 (:apriori %)) mov-hyps)))
                  sensor-from-hyps)))))

