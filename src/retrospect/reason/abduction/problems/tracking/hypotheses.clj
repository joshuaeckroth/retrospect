(ns retrospect.reason.abduction.problems.tracking.hypotheses
  (:require [clojure.string :as str])
  (:use [clojure.contrib.seq :only [find-first]])
  (:use [retrospect.reason.abduction.workspace :only [new-hyp]])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.problems.tracking.colors])
  (:use [retrospect.reason.abduction.problems.tracking.evaluate
         :only [hyps-equal?]])
  (:use [retrospect.problems.tracking.movements
         :only [dist dets-match?]])
  (:use [retrospect.reason.abduction.problems.tracking.pathsgraph
         :only [get-paths build-paths-graph path-str]])
  (:use [retrospect.state]))

(defn make-sensor-hyps
  [sensor {:keys [x y color time] :as det} t time-prev time-now]
  (let [desc (format (str "Sensor detection by %s - color: %s, x: %d, y: %d, time: %d")
                     (:id sensor) (color-str color) x y time)
        from (new-hyp "SensFrom" :sensor :sensor-from true nil 1.0 [] []
                      (format "%d,%d@%d" x y time) desc
                      {:sensor sensor :det det})
        to (new-hyp "SensTo" :sensor :sensor-to true nil 1.0 [] []
                    (format "%d,%d@%d" x y time) desc
                    {:sensor sensor :det det})]
    (cond (= t time-prev) [to]
          (= t time-now) [from]
          :else [from to])))

(defn score-movement
  "Returns nil if not matched or not in range."
  [to from walk-dist]
  (let [{x1 :x y1 :y t1 :time c1 :color :as det} (:det to)
        {x2 :x y2 :y t2 :time c2 :color :as det2} (:det from)]
    (when (and (= (inc t1) t2)
               (match-color? c1 c2))
      (let [d (dist x1 y1 x2 y2) 
            dist-count (get walk-dist d)]
        (if dist-count (double (/ dist-count (:walk-count (meta walk-dist))))
            ;; if we don't have learning, make possible movements worth
            ;; a tiny amount if the model doesn't have a frequency for
            ;; this distance
            (if (<= d (* (Math/sqrt 2) (:MaxWalk params)))
              (double (/ 1 (:walk-count (meta walk-dist))))))))))

(defn avg
  [vals]
  (double (/ (reduce + 0.0 vals) (count vals))))

(defn conflicts [h1 h2]
  (and (= :movement (:type h1) (:type h2))
       (or (and (= (:x (:det h1)) (:x (:det2 h2)))
                (= (:time (:det h1)) (:time (:det2 h2))))
           (and (= (:x (:det2 h1)) (:x (:det h2)))
                (= (:time (:det2 h1)) (:time (:det h2)))))
       (or (and (= (:y (:det h1)) (:y (:det2 h2)))
                (= (:time (:det h1)) (:time (:det2 h2))))
           (and (= (:y (:det2 h1)) (:y (:det h2)))
                (= (:time (:det2 h1)) (:time (:det h2)))))
       (or (not= (:color (:det2 h1)) (:color (:det h2)))
           (not= (:color (:det h1)) (:color (:det2 h2))))))

(defn read-walk-dist
  [file]
  (let [lines (str/split-lines (slurp file))
        walk-count (Integer/parseInt (first lines))]
    (with-meta (reduce #(assoc %1 (Double/parseDouble (first %2))
                               (Integer/parseInt (second %2)))
                       {} (map #(str/split % #",") (rest lines)))
               {:walk-count walk-count})))

(defn generate-kb
  [training]
  [(new-hyp "KB" :kb :kb false conflicts 1.0 [] [] "" ""
            {:walk-dist (read-walk-dist (format "%s/tracking/walks-%d.txt"
                                                @datadir (:MaxWalk params)))})])

(defn get-walk-dist
  [accepted]
  (:walk-dist (first (get accepted :kb))))

(defmulti hypothesize
  (fn [evidence accepted rejected hyps] [(:type evidence) (:subtype evidence)]))

(defmethod hypothesize :default [_ _ _ _] [])

(defn filter-existing
  [hyps hs]
  (filter (fn [h] (not-any? (fn [h2] (hyps-equal? h h2)) (get hyps (:type h)))) hs))

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
  (letfn [(valid? ([h] (let [c (connecting-movs h acc-mov-hyps)]
                         (or (empty? c)
                             (some #(dets-match? (:det h) (:det2 %)) c)
                             (some #(dets-match? (:det %) (:det2 h)) c)))))]
    (filter valid? mov-hyps)))

(defn new-mov-hyp
  [to from apriori det-color det2-color]
  (new-hyp "Mov" :movement :movement false conflicts
           apriori [to from] [] (path-str [det-color det2-color])
           (format "%s (dist=%.2f)"
                   (path-str [det-color det2-color])
                   (dist (:x det-color) (:y det-color)
                         (:x det2-color) (:y det2-color)))
           {:det det-color :det2 det2-color
            :mov {:x (:x det2-color) :y (:y det2-color) :time (:time det2-color)
                  :ox (:x det-color) :oy (:y det-color) :ot (:time det-color)
                  :color (:color det-color)}}))

(defn new-mov-hyps
  [to from apriori acc-mov-hyps]
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
      (map #(new-mov-hyp to from apriori
                         (assoc det-color :color %)
                         (assoc det2-color :color %))
           [red green blue])
      [(new-mov-hyp to from apriori det-color det2-color)])))

(defmethod hypothesize [:sensor :sensor-from]
  [evidence accepted rejected hyps]
  (let [sm (fn [h] (score-movement h evidence (get-walk-dist hyps)))
        acc-mov-hyps (get accepted :movement)
        nearby (filter second (map (fn [h] [h (sm h)])
                                   (filter #(= :sensor-to (:subtype %))
                                           (get accepted :sensor))))
        mov-hyps (filter-existing
                  hyps (mapcat (fn [[h apriori]]
                                 (new-mov-hyps h evidence apriori acc-mov-hyps))
                               nearby))]
    (filter-valid-movs mov-hyps acc-mov-hyps)))

(defmethod hypothesize [:sensor :sensor-to]
  [evidence accepted rejected hyps]
  (let [sm (fn [h] (score-movement evidence h (get-walk-dist accepted)))
        acc-mov-hyps (get accepted :movement)
        nearby (filter second (map (fn [h] [h (sm h)])
                                   (filter #(= :sensor-from (:subtype %))
                                           (get accepted :sensor))))
        mov-hyps (filter-existing
                  hyps (mapcat (fn [[h apriori]]
                                 (new-mov-hyps evidence h apriori acc-mov-hyps))
                               nearby))]
    (filter-valid-movs mov-hyps acc-mov-hyps)))

(defn score-path
  [mov-hyps]
  (avg (map :apriori mov-hyps)))

(defmethod hypothesize [:movement :movement]
  [evidence accepted rejected hyps]
  (let [pg (build-paths-graph (get accepted :movement))
        paths (get-paths pg evidence)]
    (filter-existing hyps (for [p paths]
                            (let [pstr (path-str (conj (vec (map :det p)) (:det2 (last p))))]
                              (new-hyp "Path" :path :path false conflicts
                                       (score-path p) p [] pstr pstr {:movs p}))))))
