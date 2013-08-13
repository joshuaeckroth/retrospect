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

(defn compute-avg-moves-dist
  [moves]
  (avg (map (fn [{:keys [ox oy x y]}] (dist ox oy x y)) moves)))

(defn generate-kb
  [training]
  [(new-hyp "KB" :kb :kb 1.0 false nil nil [] "" ""
            {:moves (:all-moves training)
             :moves-dist {:avg-moves-dist (compute-avg-moves-dist (:all-moves training))}})])

(defn get-kb
  [accepted]
  (first (get accepted :kb)))

;; TODO
(defn update-kb
  [accepted unexplained hypotheses]
  (let [mov-hyps (:movement accepted)
        old-kb (get-kb accepted)
        kb-moves (update-in old-kb [:moves] concat (map :mov mov-hyps))
        kb-moves-dist (assoc-in kb-moves [:moves-dist :avg-moves-dist] (compute-avg-moves-dist (:moves kb-moves)))]
    [kb-moves-dist]))

(defn move-prob
  [det det2 moves-dist]
  (cond (= "avg-detscores" (:MovementApriori params))
        (/ (+ (:detscore det) (:detscore det2)) 2.0)
        (= "dist" (:MovementApriorih params))
        (/ 1.0 (+ 1.0 (dist (:x det) (:y det) (:x det2) (:y det2))))
        (= "dist-diff" (:MovementApriori params))
        (let [d (dist (:x det) (:y det) (:x det2) (:y det2))]
          (- 1.0 (/ (Math/abs (- d (:avg-moves-dist moves-dist)))
                    (+ (Math/abs (- d (:avg-moves-dist moves-dist))) d))))))

(defn match-objid?
  [objid1 objid2]
  (or (nil? objid1) (nil? objid2) (= objid1 objid2)))

(defn dets-match?
  [det det2]
  (and (= (:x det) (:x det2))
       (= (:y det) (:y det2))
       (= (:time det) (:time det2))
       (match-objid? (:objid det) (:objid det2))))

(defn conflicts?
  [h1 h2]
  (or
   (and
    (= :observation (:type h1) (:type h2))
    (:objid (:det h1))
    (:objid (:det h2))
    (= (:objid (:det h1)) (:objid (:det h2)))
    (not (dets-match? (:det h1) (:det h2))))
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
            (not (match-objid? (:objid mov1) (:objid mov2))))
       ;; mov2 ends where mov1 starts and not same objid
       (and (= (:x mov2) (:ox mov1))
            (= (:y mov2) (:oy mov1))
            (= (:time mov2) (:ot mov1))
            (not (match-objid? (:objid mov2) (:objid mov1))))
       ;; same objid (not nil), same start-end times but not start-end locations
       (and (:objid mov1)
            (:objid mov2)
            (= (:objid mov1) (:objid mov2))
            (or (and (= (:time mov1) (:ot mov2))
                     (or (not= (:x mov1) (:ox mov2))
                         (not= (:y mov1) (:oy mov2))))
                (and (= (:ot mov1) (:time mov2))
                     (or (not= (:ox mov1) (:x mov2))
                         (not= (:oy mov1) (:y mov2))))))
       ;; same objid (not nil), same time, different paths
       (and (:objid mov1)
            (:objid mov2)
            (= (:objid mov1) (:objid mov2))
            (or (= (:time mov1) (:time mov2))
                (= (:ot mov1) (:ot mov2)))))))))

(defn make-sensor-hyp
  [{:keys [x y time detscore objid] :as det} from-to other-dets]
  (new-hyp (format "Sens%s" (if (= :from from-to) "From" "To"))
           :observation from-to (:detscore det)
           true nil nil []
           (format "%.2f, %.2f @ %d" x y time)
           (format "Sensor detection - x: %.2f, y: %.2f, time: %d, detscore: %.2f, objid: %s"
                   x y time detscore objid)
           {:det det}))

(defn make-sensor-hyps
  [sensors time-prev time-now accepted hypotheses]
  (let [kb (get-kb accepted)
        acc-dets (map :det (:observation accepted))
        sensed-dets (mapcat (fn [t] (sensed-at (first sensors) t))
                            (range time-prev (inc time-now)))
        prior-next-dets (concat acc-dets sensed-dets)
        prior-dets (filter #(= (dec time-now) (:time %)) prior-next-dets)
        next-dets (filter #(= time-now (:time %)) prior-next-dets)
        already-observed-dets (set (map (fn [h] [(:det h) (:subtype h)]) (:observation hypotheses)))
        to-time (if (:SequentialSensorReports params) time-prev 0)
        from-time (if (:SequentialSensorReports params) time-now (:Steps params))]
    (doall
     (if (= time-prev time-now) []
         (mapcat (fn [det] (cond
                            ;; if det has time 0 or time-prev, only generate "to" report
                            (and (= (:time det) to-time)
                                 (not (already-observed-dets [det :to])))
                            [(make-sensor-hyp det :to next-dets)]
                            ;; if det has time equal to steps or time-now,
                            ;; only generate "from" report
                            (and (= (:time det) from-time)
                                 (not (already-observed-dets [det :from])))
                            [(make-sensor-hyp det :from prior-dets)]
                            ;; otherwise, generate both "from" and "to" reports
                            (and (not= (:time det) to-time)
                                 (not= (:time det) from-time)
                                 (not (already-observed-dets [det :to]))
                                 (not (already-observed-dets [det :from])))
                            [(make-sensor-hyp det :to next-dets)
                             (make-sensor-hyp det :from prior-dets)]
                            :else []))
                 (sort-by :time sensed-dets))))))

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
        (letfn [(valid?
                  ([h] (let [c (connecting-movs h acc-mov-hyps)
                             prior-same-objid
                             (set (filter #(and (or (= (:time (:det h)) (:time (:det2 %)))
                                                    (= (:time (:det2 h)) (:time (:det %))))
                                                (= (:objid (:det h)) (:objid (:det %))))
                                          acc-mov-hyps))]
                         ;; if this mov-hyp (h) has an objid, and
                         ;; we have already accepted a mov with
                         ;; that objid, and that prior-belief
                         ;; does not match up as a connecting
                         ;; mov, this mov-hyp is not valid
                         (if (and (:objid (:det h)) (not-empty prior-same-objid))
                           (some prior-same-objid c)
                           (or (empty? c)
                               (some #(dets-match? (:det h) (:det2 %)) c)
                               (some #(dets-match? (:det %) (:det2 h)) c))))))]
          (filter valid? mov-hyps))))

(defn new-mov-hyp
  "Returns nil if the objids don't match."
  [to from acc-mov-hyps moves-dist]
  (prof :new-mov-hyp
        (let [det (:det to) det2 (:det from)
              objids-in (set (map (comp :objid :det2)
                                (connecting-movs {:det det :det2 det2} acc-mov-hyps)))
              objids-out (set (map (comp :objid :det)
                                 (connecting-movs {:det det :det2 det2} acc-mov-hyps)))
              det-objid (cond (and (= nil (:objid det))
                                   (not= nil (:objid det2)))
                              (assoc det :objid (:objid det2))
                              (and (= nil (:objid det))
                                   (= 1 (count objids-in)))
                              (assoc det :objid (first objids-in))
                              :else det)
              det2-objid (cond (and (= nil (:objid det2))
                                    (not= nil (:objid det)))
                               (assoc det2 :objid (:objid det))
                               (and (= nil (:objid det2))
                                    (= 1 (count objids-in)))
                               (assoc det2 :objid (first objids-in))
                               :else det2)
              d (dist (:x det-objid) (:y det-objid)
                      (:x det2-objid) (:y det2-objid))
              apriori (move-prob det-objid det2-objid moves-dist)]
          (when (match-objid? (:objid det-objid) (:objid det2-objid))
            (new-hyp "Mov" :movement :movement apriori false
                     (if (not= nil (:objid det-objid))
                       [(:objid det-objid) (dissoc det :objid) (dissoc det2 :objid)]
                       [(dissoc det :objid) (dissoc det2 :objid)])
                     conflicts? (map :contents [to from])
                     (format "%.2f,%.2f->%.2f,%.2f @ %d->%d (%s->%s)"
                        (:x det-objid) (:y det-objid)
                        (:x det2-objid) (:y det2-objid)
                        (:time det-objid) (:time det2-objid)
                        (str (:objid det-objid))
                        (str (:objid det2-objid)))
                     (format "%.2f,%.2f -> %.2f,%.2f (dist=%.2f) at time %d->%d (%s->%s)"
                        (:x det-objid) (:y det-objid)
                        (:x det2-objid) (:y det2-objid)
                        d (:time det-objid) (:time det2-objid)
                        (str (:objid det-objid))
                        (str (:objid det2-objid)))
                     {:det det-objid :det2 det2-objid
                      :mov {:x (:x det2-objid) :y (:y det2-objid) :time (:time det2-objid)
                            :ox (:x det-objid) :oy (:y det-objid) :ot (:time det-objid)
                            :objid (:objid det-objid)}}
                     ;; specify hyp "contents" to be just the locations/times so that
                     ;; if the objid is updated later, it overwrites this hyp
                     {:mov {:x (:x det2-objid) :y (:y det2-objid) :time (:time det2-objid)
                            :ox (:x det-objid) :oy (:y det-objid) :ot (:time det-objid)}})))))

(defn dets-nearby?
  [to from moves-dist]
  (let [det (:det to)
        det2 (:det from)
        d (dist (:x det2) (:y det2) (:x det) (:y det))]
    (and (< d (* 2.0 (:avg-moves-dist moves-dist)))
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
                          nearby (filter #(dets-nearby? evidence % (:moves-dist kb)) sensor-to-hyps)
                          mov-hyps (doall
                                    (filter identity
                                            (map #(new-mov-hyp % evidence acc-mov-hyps
                                                               (:moves-dist kb))
                                                 nearby)))]
                      (filter-valid-movs mov-hyps acc-mov-hyps)))
                  sensor-from-hyps)))))

(defn suggest-related-evidence
  [obs possible-evidence accepted]
  (let [kb (get-kb accepted)
        moves-dist (:moves-dist kb)]
    (if (= :from (:subtype obs))
      (filter (fn [obs2] (and (= :to (:subtype obs2))
                              (dets-nearby? obs obs2 moves-dist)))
              possible-evidence)
      (filter (fn [obs2] (and (= :from (:subtype obs))
                              (dets-nearby? obs2 obs moves-dist)))
              possible-evidence))))
