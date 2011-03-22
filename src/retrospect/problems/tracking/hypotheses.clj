(ns retrospect.problems.tracking.hypotheses
  (:use [retrospect.epistemicstates :only [add-hyp add-fact]])
  (:use [retrospect.workspaces :only [new-hyp]])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.colors])
  (:use [retrospect.confidences])
  (:use [retrospect.problems.tracking.grid :only [grid-at]])
  (:use [clojure.contrib.math :as math])
  (:require [clojure.set :as set :only [intersection difference]]))

(defn make-sensor-hyp
  [sensor e]
  (let [hyp (new-hyp "SH" :sensor nil NEUTRAL
                     (format "Sensor detection by %s - color: %s, x: %d, y: %d, time: %d"
                             (:id sensor) (color-str (:color (meta e)))
                             (:x (meta e)) (:y (meta e)) (:time (meta e)))
                     {:sensor sensor :entity e})]
    (with-meta e (merge (meta e) {:hyp hyp}))))

(defn sensors-to-spotted
  [sensors time sensors-seen-grid]
  (let [width (:width (meta sensors-seen-grid))
        height (:height (meta sensors-seen-grid))]
    (with-meta
      (doall (for [y (range height) x (range width)] ;; need height followed by width
               (mapcat (fn [s]
                         (map #(make-sensor-hyp s %)
                              (filter (fn [e] (and (= x (:x (meta e)))
                                                   (= y (:y (meta e)))))
                                      (sensed-at s time))))
                       sensors)))
      {:width width :height height :time time})))

(defn process-sensors
  [ep-state sensors time-now]
  (let [sensors-seen-grid (:sensors-seen-grid (:problem-data ep-state))]
    (loop [t (:time ep-state)
           sg (:spotted-grid (:problem-data ep-state))
           ep ep-state]
      (if (> t time-now)
        (update-in ep [:problem-data] assoc :spotted-grid sg)
        (let [spotted (sensors-to-spotted sensors t sensors-seen-grid)]
          (recur (inc t) (conj sg spotted)
                 (reduce #(add-fact %1 %2 []) ep
                         (map (comp :hyp meta) (flatten spotted)))))))))

(defn dist
  [x1 y1 x2 y2]
  (math/sqrt (+ (* (- x1 x2) (- x1 x2))
                (* (- y1 y2) (- y1 y2)))))

(defn score-distance
  [x1 y1 x2 y2 maxwalk]
  (let [d (dist x1 y1 x2 y2)
        mw (* (math/sqrt 2.1) maxwalk)]
    (cond
     (<= d (/ mw 4.0)) VERY-PLAUSIBLE
     (<= d (/ mw 2.0)) PLAUSIBLE
     (<= d mw) NEUTRAL)))

(defn score-path
  "spotted is a collection of sensor detections; count-seen is how
  many sensors should have seen the same thing."
  [path label maxwalk]
  (let [move-pairs (doall (partition 2 (interleave path (rest path))))]
    (if (empty? move-pairs) NEUTRAL
        (apply min (doall (map (fn [[a b]] (score-distance (:x (meta (first a)))
                                                           (:y (meta (first a)))
                                                           (:x (meta (first b)))
                                                           (:y (meta (first b)))
                                                           maxwalk))
                               move-pairs))))))

(defn path-to-movements
  [path]
  (map (fn [[e1 e2]] {:ox (:x (meta e1)) :oy (:y (meta e1)) :ot (:time (meta e1))
                      :x (:x (meta e2)) :y (:y (meta e2)) :t (:time (meta e2))})
       (doall (partition 2 (interleave (butlast (map first path))
                                       (rest (map first path)))))))

(defn path-str
  [path]
  (let [fmt #(format "%d,%d@%d " (:x (meta %)) (:y (meta %)) (:time (meta %)))]
    (str "[" (apply str (map (fn [es] (fmt (first es))) path)) "]")))

(defn paths-str
  [paths]
  (apply str (interpose "\n" (map (fn [k] (str k ":" (path-str (k paths))))
                                  (sort (keys paths))))))

(defn covered?
  [es x y time]
  (some (fn [e] (and (= (:time (meta e)) time)
                     (= (:x (meta e)) x)
                     (= (:y (meta e)) y)))
        es))

(defn find-uncovered-pos
  "Finds the earliest spotted positions that have not been covered."
  [es spotted-grid]
  (if (empty? spotted-grid) []
      (let [grid (first spotted-grid)
            time (:time (meta grid))
            uncovered
            (doall (filter identity
                           ;; need height followed by width
                           (for [y (range (:height (meta grid))) 
                                 x (range (:width (meta grid)))]
                             (if (and (not-empty (grid-at grid x y))
                                      (not (covered? es x y time)))
                               {:x x :y y :time time}))))]
        (if (not-empty uncovered) uncovered
            (recur es (rest spotted-grid))))))

(defn new-label
  [labels spotted]
  (let [nth (inc (apply max -1 (map (comp :nth meta) labels)))
        sym
        (if (empty? labels) (symbol "A")
            (loop [i nth id ""]
              (if (<= i 25) (symbol (str id (char (+ 65 i))))
                  (recur (int (- i 26)) (str id (char (+ 65 (mod i 26))))))))
        color (if (not-any? (fn [s] (not= gray (:color (meta s)))) spotted) gray
                  (:color (meta (first (filter (fn [s] (not= gray (:color (meta s))))
                                               spotted)))))]
    (with-meta sym {:color color :nth nth})))

;; TODO: Does not support sensor noise: e.g., one sensor reporting
;; red, another blue, for same position/time

(defn label-matches?
  [label spotted]
  (or (= (:color (meta label)) gray) (= (:color (meta spotted)) gray)
      (= (:color (meta label)) (:color (meta spotted)))))

(defn matched-and-in-range?
  [label e x y time prior-covered maxwalk]
  (and (label-matches? label e)
       (= (inc time) (:time (meta e)))
       (not-any? #(and (= (:time (meta %)) (:time (meta e)))
                       (= (:x (meta %)) (:x (meta e)))
                       (= (:y (meta %)) (:y (meta e))))
                 prior-covered)
       (>= (* maxwalk (math/sqrt 2.1)) (dist x y (:x (meta e)) (:y (meta e))))))

(defn spotted-in-range
  [label path prior-covered spotted-grid maxwalk]
  (let [{x :x y :y t :time} (meta (first (last path)))
        grid (first spotted-grid)]
    (doall (filter (fn [es] (some #(matched-and-in-range? label % x y t prior-covered maxwalk)
                                  es)) grid))))

(defn extend-path
  [label path prior-covered spotted-grid maxwalk]
  (let [in-range (spotted-in-range label path prior-covered spotted-grid maxwalk)]
    (doall (map (fn [e] (conj path e)) in-range))))

(defn extend-paths
  [label paths prior-covered spotted-grid maxwalk]
  (doall (mapcat (fn [p] (extend-path label p prior-covered spotted-grid maxwalk)) paths)))

(defn make-label-path
  "The first grid in spotted-grid is the time step that should be
  connected; on the recur, strip off the first grid; if spotted-grid
  has no more grids, we're done."
  [label paths prior-covered spotted-grid maxwalk]
  (if (empty? spotted-grid) paths
      (let [ex-paths (extend-paths label paths prior-covered spotted-grid maxwalk)]
        (if (empty? ex-paths) paths
            (recur label ex-paths prior-covered (rest spotted-grid) maxwalk)))))

(defn assoc-label-path
  [label paths prior-covered spotted-grid maxwalk]
  (let [prior-paths (label paths)
        last-time (apply min (map (comp :time meta first last) prior-paths))
        sg (if-not prior-paths spotted-grid (subvec spotted-grid (inc last-time)))
        new-paths (make-label-path label prior-paths prior-covered sg maxwalk)]
    ;; check if no progress was made in extending the paths;
    ;; if there was no progress, forget about the label
    (if (= new-paths prior-paths)
      (dissoc paths label)
      (assoc paths label new-paths))))

(defn make-hyp
  "Returns [hyp explains] vector."
  [path label maxwalk whereto-hyps]
  ;; add the whereto-hyp as explained, if there is a whereto-hyp for this label
  (let [explains (concat (if (whereto-hyps label) [(whereto-hyps label)] [])
                         (filter identity (map (comp :hyp meta) (flatten path))))]
    [(new-hyp "TH" :tracking label (score-path path label maxwalk)
              (str label ":" (path-str path)
                   "\nExplains: " (apply str (interpose ", " (map :id explains))))
              {:label label :path path})
     explains]))

(defn make-whereto-hyps
  [labels]
  (let [hyps (map #(new-hyp "THW" :tracking nil NEUTRAL
                            (str "Where did " % " go?") nil) labels)]
    (zipmap labels hyps)))

(defn hypothesize
  [ep-state sensors time-now params]
  (let [ep (process-sensors ep-state sensors time-now)
        whereto-hyps (make-whereto-hyps (keys (:paths (:problem-data ep-state))))
        ep-whereto (reduce #(add-fact %1 %2 []) ep (vals whereto-hyps))
        spotted-grid (:spotted-grid (:problem-data ep))
        maxwalk (:MaxWalk params)
        spotted-at (fn [{x :x y :y t :time}] (grid-at (nth spotted-grid t) x y))
        ;; put all existing paths into vectors so that alt paths can be added
        oldpaths (reduce (fn [paths l] (assoc paths l [(l paths)]))
                         (:paths (:problem-data ep-state))
                         (keys (:paths (:problem-data ep-state))))
        prior-covered (flatten (vals oldpaths))]
    (loop [paths (reduce (fn [p l] (assoc-label-path l p prior-covered spotted-grid maxwalk))
                         oldpaths (keys oldpaths))]
      (let [uncovered (find-uncovered-pos (concat (flatten (vals paths)) prior-covered)
                                          spotted-grid)]
        ;; if nothing's uncovered or we should not be creating new labels
        (if (or (empty? uncovered)
                (and (= 0 (:ProbNewEntities params)) (= 100 (:SensorCoverage params))
                     (not= 0 time-now)))
          (reduce (fn [ep [hyp explains]] (add-hyp ep hyp explains))
                  ep-whereto
                  (mapcat (fn [l] (map #(make-hyp % l maxwalk whereto-hyps) (l paths)))
                          (keys paths)))
          ;; when making a new label, consider oldpaths labels plus newpaths labels,
          ;; since the newpaths (called 'paths' here) may have dissoc'd some labels
          ;; that could not be extended
          (let [label (new-label (concat (keys oldpaths) (keys paths))
                                 (spotted-at (first uncovered)))
                newpaths (assoc paths label [[(spotted-at (first uncovered))]])
                ;; do a merge because the assoc-label-path func may dissoc the label
                ;; if no extension progress is made; since we just created
                ;; a new label we want to be sure to save the new label even if
                ;; no extension was made
                expaths (merge newpaths (assoc-label-path label newpaths prior-covered
                                                          spotted-grid maxwalk))]
            (recur expaths)))))))

(defn entity-meta
  [e]
  (select-keys (meta e) [:x :y :time]))

(defn entity-metas
  [es]
  (doall (map entity-meta es)))

(defn find-label-splits
  "Extract the terminating subpaths that are not shared among all the paths"
  [paths]
  (cond (or (empty? paths) (some #(empty? %) paths)) paths
        (< 1 (count (distinct (flatten (map (comp entity-metas first) paths))))) paths
        :else (recur (map rest paths))))

(defn find-merges
  [paths]
  (if (= 1 (count paths)) []
      (loop [ps paths
             shared []]
        (cond (or (empty? ps) (some #(empty? %) ps)) shared
              (< 1 (count (distinct (flatten (map (comp entity-metas last) ps))))) shared
              :else (recur (map butlast ps) (conj shared (last (first ps))))))))

(defn sort-paths
  [path1 path2]
  (cond
   (< (:time (meta (first (last path1)))) (:time (meta (first (last path2))))) -1
   (< (:time (meta (first (last path2)))) (:time (meta (first (last path1))))) 1
   (< (count path1) (count path2)) -1
   (< (count path2) (count path1)) 1
   :else 0))

(defn new-label-from-candidates
  [candidates paths]
  (let [labeled (distinct (entity-metas (flatten (vals paths))))
        filter-labeled (fn [path]
                         (filter (fn [es] (let [em (first (distinct (entity-metas es)))]
                                            (not-any? #(= % em) labeled))) path))
        unlabeled-path (fn [hyp] (vec (filter-labeled (:path (:data hyp)))))
        maybe-splits
        (doall (for [l (keys paths)]
                 (filter not-empty (find-label-splits
                                    (map unlabeled-path (filter #(= l (:label (:data %)))
                                                                candidates))))))
        splits (sort-by #(:time (ffirst %)) (filter not-empty maybe-splits))
        maybe-merges
        (doall (for [last-pos (distinct (flatten (map (comp entity-metas last)
                                                      (map unlabeled-path candidates))))]
                 (find-merges (filter (fn [path] (some #(= last-pos %)
                                                       (entity-metas (last path))))
                                      (map unlabeled-path candidates)))))
        merges (sort sort-paths (filter not-empty maybe-merges))]
    ;; always attempt to incorporate a split first
    (if (not-empty splits)
      (let [split (first splits)]
        (reduce (fn [ps s] (assoc ps (new-label (keys ps) (flatten s)) s))
                paths (map vec split)))
      (if (empty? merges) paths
          (let [merge (first merges)]
            (assoc paths (new-label (keys paths) (flatten merge))
                   (map vec merge)))))))

(defn commit-accepted
  [pdata accepted]
  (reduce (fn [pd hyp] (let [l (:label (:data hyp)) path (:path (:data hyp))]
                         (update-in pd [:paths] assoc l path)))
          pdata accepted))

(defn commit-decision
  [pdata accepted rejected candidates unexplained]
  (let [pd (commit-accepted pdata accepted)]
    ;; add labels for merges/splits as long as there are any
    (loop [paths (:paths pd)]
      (let [ps (new-label-from-candidates candidates paths)]
        (if (empty? (set/difference (set (keys ps)) (set (keys paths))))
          (assoc pd :paths paths)
          (recur ps))))))
