(ns retrospect.problems.tracking.hypotheses
  (:use [retrospect.epistemicstates :only [add-hyp add-fact]])
  (:use [retrospect.workspaces :only [new-hyp]])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.colors])
  (:use [retrospect.confidences])
  (:use [retrospect.problems.tracking.grid :only [grid-at dist]])
  (:use [clojure.contrib.seq :only [find-first]])
  (:require [clojure.contrib.math :as math])
  (:require [clojure.set :as set :only [intersection difference]]))

(defn make-sensor-hyps
  [sensor e]
  (let [desc (format (str "Sensor detection by %s - color: %s, "
                          "x: %d, y: %d, time: %d")
                     (:id sensor) (color-str (:color (meta e)))
                     (:x (meta e)) (:y (meta e)) (:time (meta e)))]
    (with-meta e (merge (meta e)
                        (zipmap [:hyp-from :hyp-to]
                                (map #(new-hyp % :sensor nil NEUTRAL desc
                                               {:sensor sensor :entity e})
                                     ;; sensor hyp 'from', sensor hyp 'to'
                                     ["SHF" "SHT"]))))))

(defn sensors-to-spotted
  [sensors time sensors-seen-grid]
  (let [width (:width (meta sensors-seen-grid))
        height (:height (meta sensors-seen-grid))]
    (with-meta
      (doall (for [y (range height) x (range width)] ;; need height followed by width
               (mapcat (fn [s]
                         (map #(make-sensor-hyps s %)
                              (filter (fn [e] (and (= x (:x (meta e)))
                                                   (= y (:y (meta e)))))
                                      (sensed-at s time))))
                       sensors)))
      {:width width :height height :time time})))

(defn process-sensors
  "For each time step between the last time we processed sensor data and the current time,
   look at the sensor detections at that time step, incorporate them into our running
   estimate of the grid, add those detections as 'facts,' and record those detections
   as 'uncovered.'"
  [ep-state sensors time-now]
  (let [sensors-seen-grid (:sensors-seen-grid (:problem-data ep-state))]
    (loop [t (inc (apply max -1 (map (comp :time meta)
                                     (flatten (:spotted-grid (:problem-data ep-state))))))
           sg (:spotted-grid (:problem-data ep-state))
           uncovered (:uncovered (:problem-data ep-state))
           ep ep-state]
      (if (> t time-now)
        (update-in ep [:problem-data] assoc :spotted-grid sg :uncovered uncovered)
        (let [spotted (sensors-to-spotted sensors t sensors-seen-grid)]
          (recur (inc t) (conj sg spotted)
                 (reduce (fn [unc sp] (conj unc (select-keys (meta sp) [:x :y :time :color])))
                         uncovered (flatten spotted))
                 (reduce #(add-fact %1 %2 [])
                         ep (concat
                             ;; get hyp-to hypotheses from prior spotted grid
                             (map (comp :hyp-to meta) (flatten (or (last sg) [])))
                             ;; and hyp-from hypotheses from current spotted grid
                             (map (comp :hyp-from meta) (flatten spotted))))))))))

(defn score-distance
  "Returns nil if movement is impossible."
  [x1 y1 x2 y2 maxwalk]
  (let [d (dist x1 y1 x2 y2)
        mw (* (math/sqrt 2.1) maxwalk)]
    (cond
     (<= d (/ mw 8.0)) NEUTRAL
     (<= d (/ mw 6.0)) PLAUSIBLE
     (<= d (/ mw 3.0)) VERY-PLAUSIBLE
     (<= d (/ mw 2.0)) NEUTRAL
     (<= d (/ (* mw 2.0) 3.0)) IMPLAUSIBLE
     (<= d mw) VERY-IMPLAUSIBLE)))

(defn score-movement
  "Returns nil if movement is impossible."
  [{x1 :x y1 :y t1 :time c1 :color} {x2 :x y2 :y t2 :time c2 :color} params]
  (score-distance x1 y1 x2 y2 (:MaxWalk params)))

(defn matched-and-in-range?
  "Returns nil if not matched or not in range."
  [{x1 :x y1 :y t1 :time c1 :color :as det} {x2 :x y2 :y t2 :time c2 :color :as det2} params]
  (when (and (= (inc t1) t2) (match-color? c1 c2))
    (when-let [score (score-movement det det2 params)]
      score)))

(defn make-movement-hyps
  [det uncovered spotted-grid params]
  (let [find-spotted (fn [d] (filter #(= (:color d) (:color (meta %)))
                                     (grid-at (nth spotted-grid (:time d))
                                              (:x d) (:y d))))
        desc-fn (fn [det det2 explains]
                  (format "%d,%d@%d -> %d,%d@%d\nExplains: %s"
                          (:x det) (:y det) (:time det)
                          (:x det2) (:y det2) (:time det2)
                          (apply str (interpose "," (map :id explains)))))]
    (filter identity
            (for [det2 uncovered]
              (when-let [score (matched-and-in-range? det det2 params)]
                ;; it is important that the hyp explains where det2 is 'from' and
                ;; where det went 'to'
                (let [explains (concat (map (comp :hyp-from meta) (find-spotted det2))
                                       (map (comp :hyp-to meta) (find-spotted det)))]
                  [(new-hyp "TH" :tracking nil score (desc-fn det det2 explains)
                            {:det det :det2 det2})
                   explains]))))))

(defn hypothesize
  "Process sensor reports, then make hypotheses for all possible movements,
   and add them to the epistemic state."
  [ep-state sensors time-now params]
  (let [ep (process-sensors ep-state sensors time-now)
        sg (:spotted-grid (:problem-data ep))
        uncovered (:uncovered (:problem-data ep))]
    (loop [hyps []
           unc uncovered]
      (if (empty? unc)
        ;; ran out of uncovered detections; so add all the hyps
        (reduce (fn [ep [hyp explains]] (add-hyp ep hyp explains)) ep hyps)
        ;; take the first uncovered detection, and make movement hyps out of it
        (let [mov-hyps (make-movement-hyps (first unc) uncovered sg params)]
          (recur (concat hyps mov-hyps) (rest unc)))))))

(defn path-str
  [path]
  (apply str (interpose " -> " (map (fn [{:keys [x y time]}]
                                      (format "%s,%s@%s" x y time)) path))))

(defn paths-str
  [paths]
  (apply str (interpose "\n" (map (fn [label] (format "%s (%s): %s"
                                                      label (color-str (:color (meta label)))
                                                      (path-str (get paths label))))
                                  (keys paths)))))

(defn path-to-movements
  [path]
  (map (fn [[det det2]]
         {:ox (:x det) :oy (:y det) :ot (:time det)
          :x (:x det2) :y (:y det2) :t (:time det2)})
       (partition 2 (interleave (butlast path) (rest path)))))

(defn paths-to-movements
  [paths]
  (flatten (map (fn [label] (path-to-movements (get paths label))) (keys paths))))

(defn active-paths
  [paths]
  (vals (select-keys paths (filter (comp not :dead meta) (keys paths)))))

(defn inactive-paths
  [paths]
  (vals (select-keys paths (filter (comp :dead meta) (keys paths)))))

(defn find-color
  [move path]
  (if-let [c (find-first #(not= gray %) (map :color (concat move path)))]
    c gray))

(defn new-label
  ([labels move path]
     (let [nth (inc (apply max -1 (map (comp :nth meta) labels)))
           sym (if (empty? labels) (symbol "A")
                   (loop [i nth id ""]
                     (if (<= i 25) (symbol (str id (char (+ 65 i))))
                         (recur (int (- i 26)) (str id (char (+ 65 (mod i 26))))))))
           color (find-color move path)]
       (with-meta sym {:color color :nth nth})))
  ([labels move] (new-label labels move [])))

(defn is-extension?
  [paths label [det det2] live?]
  (when (or (not live?) (not (:dead (meta label))))
    (let [last-det (last (get paths label))]
      (and (= (select-keys last-det [:x :y :time])
              (select-keys det [:x :y :time]))
           (match-color? (:color det2) (:color (meta label)))))))

(defn update-label-color
  "Update color if the label is gray."
  [paths label move]
  (if (not= gray (:color (meta label))) label
      (let [path (get paths label)
            color (find-color move path)]
        (with-meta label (merge (meta label) {:color color})))))

(defn extend-paths
  "Given a movement, try to extend an existing path to incorporate that movement.
   Clearly, the only valid path to extend is the one that ends where
   the movement begins, and such a path should only be extended if the
   movement is not involved in a split or merge. If a movement cannot
   be incorporated, mark it as 'bad'."
  [splits merges {:keys [paths bad]} move]
  (cond
   ;; if we have a split, just mark this label dead
   (some #{move} splits)
   (if-let [label (find-first #(is-extension? paths % move true) (keys paths))]
     {:paths (-> paths
                 (dissoc label)
                 (assoc (with-meta label (merge (meta label) {:dead true}))
                   (get paths label)))
      :bad bad}
     ;; no label found, return paths
     {:paths paths :bad bad})
   ;; if we have a merge, continue the relevant path one step, then call it dead
   (some #{move} merges)
   (if-let [label (find-first #(is-extension? paths % move false) (keys paths))]
     ;; if we found what has been merged, process it
     (let [path (get paths label)
           label-color (update-label-color paths label move)
           label-dead (with-meta label-color (merge (meta label-color) {:dead true}))]
       {:paths (-> paths
                   (dissoc label)
                   (assoc label-dead (conj path (second move))))
        :bad bad})
     ;; otherwise just return paths
     {:paths paths :bad bad})
   ;; otherwise, no split/merge, so find the 'live' extension
   :else
   (if-let [label (find-first #(is-extension? paths % move true) (keys paths))]
     (let [path (get paths label)
           label-color (update-label-color paths label move)]
       {:paths (-> paths
                   (dissoc label)
                   (assoc label-color (conj path (second move))))
        :bad bad})
     ;; this movement continues no known path, so make a new label
     (if (or (empty? paths) (= 0 (:time (first move))))
       {:paths (assoc paths (new-label (keys paths) move) move) :bad bad}
       {:paths paths :bad (conj bad move)}))))

(defn split-path
  "A split has a common first det and a unique second det2; we want to
   create a new label for the det2, and give that label the color
   discovered before the split occurred."
  [paths move]
  (if (some #{(second move)} (flatten (active-paths paths)))
    ;; split has already been incorporated; return original paths
    paths
    ;; otherwise, make our new label; find the path this split continues
    ;; to obtain color information
    (let [prior-path (find-first (fn [path] (= (last path) (first move)))
                                 (inactive-paths paths))]
      (assoc paths (new-label (keys paths) move prior-path) move))))

(defn merge-path
  "A merge has a common second det2 and a unique first det. We don't want
   to make a new label for each of the unique first det's; we only want
   one new label for each merge. This function accepts a single movement,
   so we need to check if the merge has already been given a new label in
   the paths map. If it has, we don't create another one."
  [paths move]
  (if (some #{(second move)} (flatten (active-paths paths)))
    ;; merge has already been incorporated; return original paths
    paths
    ;; otherwise, make our new label; we gather all prior paths to obtain
    ;; color information
    (let [prior-paths (apply concat (filter (fn [path] (= (last path) (second move)))
                                            (inactive-paths paths)))]
      (assoc paths (new-label (keys paths) move prior-paths) [(second move)]))))

(defn move-splits?
  [moves move]
  (some (fn [[det det2]] (and (= det (first move)) (not= det2 (second move)))) moves))

(defn move-merges?
  [moves move]
  (some (fn [[det det2]] (and (not= det (first move)) (= det2 (second move)))) moves))

(defn commit-decision
  [pdata accepted rejected shared-explains unexplained time-now]
  (if (empty? accepted) pdata
      (let [moves (map (fn [h]
                         (with-meta [(:det (:data h)) (:det2 (:data h))] {:hyp h}))
                       (sort-by (comp :time :det :data) accepted))
            ;; find splits and merges
            splits (filter (partial move-splits? moves) moves)
            merges (filter (partial move-merges? moves) moves)
            maxtime (dec (apply max (map :time (flatten moves))))]
        ;; incorporate the decision one time step at a time
        (loop [t (apply min (map :time (flatten moves)))
               paths (:paths pdata)
               bad #{}]
          (if (> t maxtime)
            (assoc pdata :paths paths :bad (map (comp :hyp meta) bad))
            (let [tmoves (filter #(= t (:time (first %))) moves)
                  tsplits (filter #(= t (:time (first %))) splits)
                  tmerges (filter #(= t (:time (first %))) merges)
                    
                  ;; extend the paths
                  {ex-paths :paths newbad :bad}
                  (reduce (partial extend-paths tsplits tmerges)
                          {:paths paths :bad bad} tmoves)
                    
                  ;; make new labels for splits
                  ;; (new label for each second det2 of movement)
                  split-paths (reduce split-path ex-paths tsplits)
                    
                  ;; make new labels for merges
                  ;; (new label for each second det2 of movement)
                  merge-paths (reduce merge-path split-paths tmerges)]
              (recur (inc t) merge-paths newbad)))))))
