(ns retrospect.problems.tracking.hypotheses
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:use [retrospect.reason.abduction.workspace :only [new-hyp]])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.problems.tracking.colors])
  (:use [retrospect.problems.tracking.movements
         :only [dist dets-match?]])
  (:use [geppetto.profile :only [prof]])
  (:use [retrospect.evaluate :only [normalize avg]])
  (:use [geppetto.random])
  (:use [retrospect.state]))

(defn compute-moves-dist
  [moves]
  (if (= "gaussian" (:WalkType params))
    (let [dists (map #(dist (:ox %) (:oy %) (:x %) (:y %)) moves)
          mean (/ (reduce + dists) (count dists))
          variance (/ (reduce + (map #(Math/pow (- mean %) 2.0) dists)) (count dists))]
      {:mean mean :variance variance})
    ;; else, :WalkType = "random"
    (let [dists (map #(dist (:ox %) (:oy %) (:x %) (:y %)) moves)
          freqs (frequencies dists)
          c (count moves)]
      {:dist-freqs freqs :count c
       :max-prob (apply max (map #(/ (double (+ 1 %)) (double (+ 2 c)))
                               (vals freqs)))
       :avg-moves-dist (avg dists)
       :max-moves-dist (apply max dists)})))

(defn generate-kb
  [training]
  [(new-hyp "KB" :kb :kb 1.0 false [] nil [] "" ""
            {:moves (:moves training)
             :moves-dist (compute-moves-dist (:moves training))
             :seen-colors (:seen-colors training)})])

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

(defn conflicts?
  [h1 h2]
  (or
   (and
    (= :observation (:type h1) (:type h2))
    (not= gray (:color (:det h1)))
    (not= gray (:color (:det h2)))
    (match-color? (:color (:det h1)) (:color (:det h2)))
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
       ;; same color (not gray), same start-end times but not start-end locations
       (and (not= gray (:color mov1))
            (not= gray (:color mov2))
            (= (:color mov1) (:color mov2))
            (or (and (= (:time mov1) (:ot mov2))
                     (or (not= (:x mov1) (:ox mov2))
                         (not= (:y mov1) (:oy mov2))))
                (and (= (:ot mov1) (:time mov2))
                     (or (not= (:ox mov1) (:x mov2))
                         (not= (:oy mov1) (:y mov2))))))
       ;; same color (not gray), same time, different paths
       (and (not= gray (:color mov1))
            (not= gray (:color mov2))
            (= (:color mov1) (:color mov2))
            (or (= (:time mov1) (:time mov2))
                (= (:ot mov1) (:ot mov2)))))))))

(defn move-prob
  [d moves-dist]
  (/ (/ (double (+ 1 (get-in moves-dist [:dist-freqs d] 0)))
        (double (+ 2 (:count moves-dist))))
     (:max-prob moves-dist)))

(defn penalize-gray-moves
  [apriori det det2]
  (if (not (:PenalizeGrayMoves params)) apriori
      (cond
       ;; one det is gray
       (or (and (= gray (:color det))
                (not= gray (:color det2)))
           (and (= gray (:color det2))
                (not= gray (:color det))))
       (* 0.75 apriori)
       ;; two dets are gray
       (and (= gray (:color det) (:color det2)))
       (* 0.5 apriori)
       ;; neither det is gray
       :else apriori)))

(defn calc-det-prob
  [det other-dets moves-dist]
  (let [move-probs (map (fn [det2] (let [d (dist (:x det2) (:y det2)
                                                 (:x det) (:y det))
                                         apriori (move-prob d moves-dist)]
                                     (penalize-gray-moves apriori det det2)))
                        (filter #(match-color? (:color %) (:color det)) other-dets))]
    (if (not-empty move-probs)
      (cond (= "avg" (:DetScore params))
            (avg move-probs)
            (= "min" (:DetScore params))
            (apply min move-probs)
            (= "max" (:DetScore params))
            (apply max move-probs))
      ;; time 0 or lost track (nothing of same color at time before);
      ;; give default apriori value
      0.5)))

(defn dets-nearby?
  [to from moves-dist]
  (let [det (:det to)
        det2 (:det from)
        d (dist (:x det2) (:y det2) (:x det) (:y det))]
    (and (< d (* 1.1 (:max-moves-dist moves-dist)))
         (= (:time (:det to)) (inc (:time (:det from)))))))

(defn make-sensor-hyp
  [{:keys [x y color time] :as det} from-to other-dets moves-dist]
  (let [apriori (or (:apriori det) (calc-det-prob det other-dets moves-dist) true)]
    (new-hyp (format "Sens%s" (if (= :from from-to) "From" "To"))
             :observation from-to apriori true
             [(keyword (format "obs-%s-%d" (name from-to) time))] conflicts? []
             (format "%d,%d@%d" x y time)
             (format (str "Sensor detection - color: %s, x: %d, y: %d, time: %d\n\nOther dets:\n%s")
                     (color-str color) x y time
                     (str/join "\n" (map str (map #(update-in % [:color] color-str)
                                                  (filter #(match-color? (:color %) color) other-dets)))))
             {:det det})))

(defn make-sensor-hyps
  [sensors time-prev time-now accepted hypotheses anomalies]
  (let [kb (get-kb accepted)
        moves-dist (:moves-dist kb)
        acc-dets (set (map :det (:observation accepted)))
        sensed-dets (set (mapcat (fn [t] (mapcat (fn [s] (sensed-at s t)) sensors))
                                 (range time-prev (inc time-now))))
        prior-next-dets (set/union acc-dets sensed-dets)
        prior-dets (filter #(= (dec time-now) (:time %)) prior-next-dets)
        next-dets (filter #(= time-now (:time %)) prior-next-dets)
        already-observed-dets (set (map (fn [h] [(:det h) (:subtype h)]) (:observation hypotheses)))
        to-time (if (:SequentialSensorReports params) time-prev 0)
        from-time (if (:SequentialSensorReports params) time-now (:Steps params))
        all-obs (if (= time-prev time-now) []
                    (mapcat (fn [det] (cond
                                       ;; if det has time 0 or time-prev, only generate "to" report
                                       (and (= (:time det) to-time)
                                            (not (already-observed-dets [det :to])))
                                       [(make-sensor-hyp det :to next-dets moves-dist)]
                                       ;; if det has time equal to steps or time-now,
                                       ;; only generate "from" report
                                       (and (= (:time det) from-time)
                                            (not (already-observed-dets [det :from])))
                                       [(make-sensor-hyp det :from prior-dets moves-dist)]
                                       ;; otherwise, generate both "from" and "to" reports
                                       (and (not= (:time det) to-time)
                                            (not= (:time det) from-time)
                                            (not (already-observed-dets [det :to]))
                                            (not (already-observed-dets [det :from])))
                                       [(make-sensor-hyp det :to next-dets moves-dist)
                                        (make-sensor-hyp det :from prior-dets moves-dist)]
                                       :else []))
                            (sort-by :time sensed-dets)))]
    (if (not-empty anomalies)
      (doall (filter (fn [obs] (if (= :from (:subtype obs))
                                 (some (fn [obs2] (and (= :to (:subtype obs2))
                                                       (dets-nearby? obs obs2 moves-dist)))
                                       anomalies)
                                 (some (fn [obs2] (and (= :from (:subtype obs2))
                                                       (dets-nearby? obs2 obs moves-dist)))
                                       anomalies)))
                     all-obs))
      (doall (filter (fn [obs] (>= (:apriori obs) (/ (:SensorThreshold params) 100.0))) all-obs)))))

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
                             prior-same-color
                             (set (filter #(and (or (= (:time (:det h)) (:time (:det2 %)))
                                               (= (:time (:det2 h)) (:time (:det %))))
                                           (= (:color (:det h)) (:color (:det %))))
                                     acc-mov-hyps))]
                         ;; if this mov-hyp (h) has a color, and
                         ;; we have already accepted a mov with
                         ;; that color, and that prior-belief
                         ;; does not match up as a connecting
                         ;; mov, this mov-hyp is not valid
                         (if (and (not= gray (:color (:det h)))
                                  (not-empty prior-same-color))
                           (some prior-same-color c)
                           (or (empty? c)
                               (some #(dets-match? (:det h) (:det2 %)) c)
                               (some #(dets-match? (:det %) (:det2 h)) c))))))]
          (filter valid? mov-hyps))))

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
                      (:x det2-color) (:y det2-color))
              apriori (move-prob d moves-dist)
              apriori-color-penalty (penalize-gray-moves apriori det det2)]
          (when (match-color? (:color det-color) (:color det2-color))
            (new-hyp "Mov" :movement :movement apriori-color-penalty false
                     (if (not= gray (:color det-color))
                       [(:color det-color) (dissoc det :color) (dissoc det2 :color)]
                       [(dissoc det :color) (dissoc det2 :color)])
                     conflicts? (map :contents [to from])
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
                            :color (:color det-color)}}
                     ;; specify hyp "contents" to be just the locations/times so that
                     ;; if the color is updated later, it overwrites this hyp
                     {:mov {:x (:x det2-color) :y (:y det2-color) :time (:time det2-color)
                            :ox (:x det-color) :oy (:y det-color) :ot (:time det-color)}})))))

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
                                                        (:moves-dist kb) (:seen-colors kb))
                                          nearby)))]
                      (filter-valid-movs mov-hyps acc-mov-hyps)))
                  sensor-from-hyps)))))
