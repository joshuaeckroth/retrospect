(ns retrospect.problems.tracking.hypotheses
  (:use [retrospect.epistemicstates :only [add-hyp add-fact]])
  (:use [retrospect.workspaces :only [new-hyp]])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.colors])
  (:use [retrospect.confidences])
  (:use [retrospect.problems.tracking.grid :only [grid-at dist]])
  (:use [clojure.contrib.seq :only [find-first]])
  (:require [clojure.contrib.math :as math])
  (:require [clojure.set :as set :only [intersection difference]])
  (:use [loom.graph :only [digraph add-edges]])
  (:use [loom.attr :only [add-attr]]))

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
      (doall (for ;; need height followed by width
                 [y (range height) x (range width)]
               (mapcat (fn [s]
                         (map #(make-sensor-hyps s %)
                              (filter (fn [e] (and (= x (:x (meta e)))
                                                   (= y (:y (meta e)))))
                                      (sensed-at s time))))
                       sensors)))
      {:width width :height height :time time})))

(defn process-sensors
  "For each time step between the last time we processed sensor data
   and the current time, look at the sensor detections at that time
   step, incorporate them into our running estimate of the grid, add
   those detections as 'facts,' and record those detections as
   'uncovered.'"
  [ep-state sensors time-now]
  (let [sensors-seen-grid (:sensors-seen-grid (:problem-data ep-state))]
    (loop [t (inc (apply max -1
                         (map (comp :time meta)
                              (flatten (:spotted-grid (:problem-data ep-state))))))
           sg (:spotted-grid (:problem-data ep-state))
           uncovered (:uncovered (:problem-data ep-state))
           ep ep-state]
      (if (> t time-now)
        (update-in ep [:problem-data] assoc :spotted-grid sg :uncovered uncovered)
        (let [spotted (sensors-to-spotted sensors t sensors-seen-grid)]
          (recur (inc t) (conj sg spotted)
                 (set/union uncovered (set (map #(select-keys
                                                  (meta %) [:x :y :time :color])
                                                (flatten spotted))))
                 (reduce #(add-fact %1 %2 [])
                         ep (concat
                             ;; get hyp-to hypotheses from prior spotted grid
                             (map (comp :hyp-to meta) (flatten (or (last sg) [])))
                             ;; and hyp-from hypotheses from current spotted grid,
                             ;; but only if the hyp's time != 0
                             (filter #(not= 0 (:time (meta (:entity (:data %)))))
                                     (map (comp :hyp-from meta)
                                          (flatten spotted)))))))))))

(defn score-distance
  "Returns nil if movement is impossible."
  [x1 y1 x2 y2 maxwalk]
  (let [d (dist x1 y1 x2 y2)
        mw (* (math/sqrt 2.1) maxwalk)]
    (cond
     (<= d (* maxwalk 0.05)) IMPLAUSIBLE
     (<= d (* maxwalk 0.15)) VERY-PLAUSIBLE
     (<= d (* maxwalk 0.20)) PLAUSIBLE
     (<= d (* maxwalk 0.25)) VERY-PLAUSIBLE
     (<= d (* maxwalk 0.45)) PLAUSIBLE
     (<= d (* maxwalk 0.55)) NEUTRAL
     (<= d mw) IMPLAUSIBLE)))

(defn score-movement
  "Returns nil if movement is impossible."
  [{x1 :x y1 :y t1 :time c1 :color}
   {x2 :x y2 :y t2 :time c2 :color}
   params]
  (score-distance x1 y1 x2 y2 (:MaxWalk params)))

(defn matched-and-in-range?
  "Returns nil if not matched or not in range."
  [{x1 :x y1 :y t1 :time c1 :color :as det}
   {x2 :x y2 :y t2 :time c2 :color :as det2}
   params]
  (when (and (= (inc t1) t2) (match-color? c1 c2))
    (when-let [score (score-movement det det2 params)]
      score)))

(defn make-movement-hyps
  [det uncovered spotted-grid params]
  (let [find-spotted (fn [d] (filter #(= (:color d) (:color (meta %)))
                                     (grid-at (nth spotted-grid (:time d))
                                              (:x d) (:y d))))
        desc-fn (fn [det det2 explains]
                  (format "%d,%d@%d (%s) -> %d,%d@%d (%s)\nExplains: %s"
                          (:x det) (:y det) (:time det)
                          (color-str (:color det))
                          (:x det2) (:y det2) (:time det2)
                          (color-str (:color det2))
                          (apply str (interpose "," (map :id explains)))))]
    (filter identity
            (for [det2 uncovered]
              (when-let [score (matched-and-in-range? det det2 params)]
                ;; it is important that the hyp explains where det2 is
                ;; 'from' and where det went 'to'
                (let [explains (concat (map (comp :hyp-from meta)
                                            (find-spotted det2))
                                       (map (comp :hyp-to meta)
                                            (find-spotted det)))]
                  [(new-hyp "TH" :tracking nil score (desc-fn det det2 explains)
                            {:det det :det2 det2})
                   explains]))))))

(defn build-paths-graph
  [hyps]
  (reduce (fn [g h]
            (let [det (:det (:data h))
                  det2 (:det2 (:data h))
                  edge [det det2]]
              (-> g (add-edges edge)
                  (add-attr det :color (color-str (:color det)))
                  (add-attr det :fontcolor (color-str (:color det)))
                  (add-attr det :label (format "%d,%d@%d"
                                               (:x det) (:y det) (:time det)))
                  (add-attr det2 :color (color-str (:color det2)))
                  (add-attr det2 :fontcolor (color-str (:color det2)))
                  (add-attr det2 :label (format "%d,%d@%d"
                                                (:x det2) (:y det2) (:time det2)))
                  (add-attr det det2 :hyp h)
                  (add-attr det det2 :label (:id h)))))
          (digraph) hyps))

(defn hypothesize
  "Process sensor reports, then make hypotheses for all possible movements,
   and add them to the epistemic state."
  [ep-state sensors time-now params]
  (let [path-heads (map last (vals (:paths (:problem-data ep-state))))
        ep (process-sensors ep-state sensors time-now)
        sg (:spotted-grid (:problem-data ep))
        uncovered (set/union (set path-heads) (:uncovered (:problem-data ep)))]
    (loop [hyps []
           unc uncovered]
      (if (empty? unc)
        ;; ran out of uncovered detections; so add all the hyps
        (let [paths-graph (build-paths-graph (map first hyps))
              pdata (assoc (:problem-data ep-state) :paths-graph paths-graph)
              ep-paths-graph (assoc ep :problem-data pdata)]
          (reduce (fn [ep [hyp explains]] (add-hyp ep hyp explains))
                  ep-paths-graph hyps))
        ;; take the first uncovered detection, and make movement hyps out of it
        (let [mov-hyps (make-movement-hyps (first unc) uncovered sg params)]
          (recur (concat hyps mov-hyps) (rest unc)))))))

(defn move-str
  [[det det2]]
  (format "%d,%d@%d (%s) -> %d,%d@%d (%s)"
          (:x det) (:y det) (:time det) (color-str (:color det))
          (:x det2) (:y det2) (:time det2) (color-str (:color det2))))

(defn path-str
  [path]
  (apply str (interpose " -> " (map (fn [{:keys [x y time]}]
                                      (format "%s,%s@%s" x y time)) path))))

(defn paths-str
  [paths]
  (apply str (map (fn [label] (format "%s%s (%s): %s\n"
                                      label
                                      (if (:dead (meta label)) "*" "")
                                      (color-str (:color (meta label)))
                                      (path-str (get paths label))))
                  (keys paths))))

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
  "Determine the color of a label given a movement and a prior
  path. If the prior path + movement (in sum, the 'path') has no
  color, than obviously the new color should be gray. Otherwise, if
  the path consists of red (and gray) but no blue, it should be blue,
  and vice versa; if the path consists of some red and some blue, the
  new path should be gray."
  ([path]
     (let [colors (map :color path)
           has-red? (some #{red} colors)
           has-blue? (some #{blue} colors)]
       (cond (and (not has-red?) (not has-blue?)) gray
             (and has-red? (not has-blue?)) red
             (and has-blue? (not has-red?)) blue
             :else gray)))
  ([move path]
     (find-color (concat move path))))

(defn new-label
  ([labels move path]
     (let [nth (inc (apply max -1 (map (comp :nth meta) labels)))
           sym (if (empty? labels) (symbol "A")
                   (loop [i nth id ""]
                     (if (<= i 25) (symbol (str id (char (+ 65 i))))
                         (recur (int (- i 26))
                                (str id (char (+ 65 (mod i 26))))))))
           color (find-color move path)]
       (with-meta sym {:color color :nth nth})))
  ([labels move] (new-label labels move [])))

(defn is-extension?
  [paths label [det det2] direction]
  (when (not (:dead (meta label)))
    (let [link-det (if (= direction :forward)
                     (last (get paths label))
                     (first (get paths label)))]
      (and (= (select-keys link-det [:x :y :time])
              (select-keys (if (= direction :forward) det det2)
                           [:x :y :time]))))))

(defn update-label-color
  "Update color if the label is gray."
  [paths label move]
  (if (not= gray (:color (meta label))) label
      (let [path (get paths label)
            color (find-color move path)]
        (with-meta label (merge (meta label) {:color color})))))

(defn extend-paths
  "Given a movement, try to extend an existing path to incorporate
   that movement. Clearly, the only valid path to extend is the one
   that ends where the movement begins, and such a path should only be
   extended if the movement is not involved in a merge (new labels
   have already been established for splits). If a movement cannot be
   incorporated, mark it as 'bad'. There may be multiple paths that
   can be extended (see the 'merge-ambiguity-gray' prepared case)."
  [splits merges alts {:keys [paths bad log]} move]
  (let [filter-labels (fn [ls] (filter #(and (match-color? (:color (first move))
                                                           (:color (meta %)))
                                             (match-color? (:color (second move))
                                                           (:color (meta %))))
                                       ls))
        maybe-before-labels (filter #(is-extension? paths % move :forward)
                                    (keys paths))
        maybe-after-labels (filter #(is-extension? paths % move :backward)
                                   (keys paths))
        before-labels (filter-labels maybe-before-labels)
        after-labels (filter-labels maybe-after-labels)
        alts-for-move
        (filter (fn [h]
                  (or (and (= (select-keys (:det (:data h)) [:x :y :time])
                              (select-keys (second move) [:x :y :time])))
                      (and (= (select-keys (:det2 (:data h)) [:x :y :time])
                              (select-keys (first move) [:x :y :time])))))
                alts)]
    (println (move-str move))
    (println "original alts:" (map :id alts))
    (println "alts for move:" (map :id alts-for-move))
    (cond
     ;; if we have a split, just mark each applicable label as dead
     (some #{move} splits)
     (if (not-empty before-labels)
       {:paths (reduce (fn [ps l]
                         (-> ps (dissoc l)
                             (assoc (with-meta l (merge (meta l) {:dead true}))
                               (get ps l))))
                       paths before-labels)
        :bad bad}
       ;; no label found, return paths
       {:paths paths :bad bad})
     ;; if we have a merge, continue the relevant paths one step,
     ;; then call them dead
     (some #{move} merges)
     (if (not-empty before-labels)
       ;; if we found what has been merged, process them
       {:paths
        (reduce (fn [ps l]
                  (let [path (get ps l)
                        l-color (update-label-color ps l move)
                        l-dead (with-meta l-color (merge (meta l-color)
                                                         {:dead true}))
                        ps-dead (-> ps (dissoc l)
                                    (assoc l-dead (conj path (second move))))
                        same-merge (find-first
                                    (fn [l] (and (= (second move)
                                                    (last (l paths)))))
                                    (filter (comp not :dead meta) (keys paths)))]
                    (if same-merge ps-dead
                        (assoc ps-dead
                          (new-label (keys ps-dead) move) [(second move)]))))
                paths before-labels)
        :log (conj log (format
                        "Merging %s with labels %s" (move-str move)
                        (apply str (interpose "," (map str before-labels)))))
        :bad bad}
       ;; no label found, so make new (dead) label to participate in the merge
       (let [label (new-label (keys paths) move)
             dead-label (with-meta label (merge (meta label) {:dead true}))]
         {:paths (assoc paths dead-label move)
          :log (conj log (format "%s is a merge, new (dead) label %s"
                                 (move-str move) dead-label))
          :bad bad}))
     ;; this movement continues no path, yet there is a label meeting
     ;; the point of the movement; additionally, there are no possible
     ;; alternative explainers for this move's head or tail; in such
     ;; cases, the colors don't match; make a new label, but mark the
     ;; movement as bad
     (and (empty? alts-for-move)
          (or (and (empty? before-labels) (not-empty maybe-before-labels))
              (and (empty? after-labels) (not-empty maybe-after-labels))))
     (let [label (new-label (keys paths) move)]
       {:paths (assoc paths label move)
        :log (conj log (format
                        "Making new label %s / *bad* / before: %s; after: %s"
                        label
                        (apply str (interpose "," (map str maybe-before-labels)))
                        (apply str (interpose "," (map str maybe-after-labels))))) 
        :bad (conj bad move)})
     ;; we don't even have an existing path coming up to touch the
     ;; movement; so, just make a new label (don't mark bad because we
     ;; have no information leading us to believe it's a bad movement,
     ;; it's just an anomaly that needs to be resolved by introducing
     ;; a label)
     (and (empty? maybe-before-labels) (empty? maybe-after-labels))
     (let [label (new-label (keys paths) move)]
       {:paths (assoc paths label move)
        :log (conj log (format "New label, nothing nearby: %s for %s"
                               label (move-str move)))
        :bad bad})
     ;; at least one prior path (before-label) can be extended, so
     ;; extend all of them
     (not-empty before-labels)
     {:paths (reduce (fn [ps l]
                       (let [path (get ps l)
                             l-color (update-label-color ps l move)]
                         (-> ps (dissoc l)
                             (assoc l-color (conj path (second move))))))
                     paths before-labels)
      :log (conj log (format "Extending %s with %s"
                             (apply str (interpose "," (map str before-labels)))
                             (move-str move)))
      :bad bad}
     ;; at least one later path (after-label) can be prepended, so
     ;; prepend all of them
     :else
     {:paths (reduce (fn [ps l]
                       (let [path (get ps l)
                             l-color (update-label-color ps l move)]
                         (-> ps (dissoc l)
                             (assoc l-color (concat [(first move)] path)))))
                     paths after-labels)
      :log (conj log (format "Prepending %s with %s"
                             (apply str (interpose "," (map str before-labels)))
                             (move-str move)))
      :bad bad})))

(defn split-path
  "A split has a common first det and a unique second det2; we want to
   create a new label for the det2, and give that label the color
   discovered before the split occurred."
  [paths move]
  (if (some #{(seq move)} (active-paths paths))
    ;; split has already been incorporated; return original paths
    paths
    ;; otherwise, make our new label; find the path this split continues
    ;; to obtain color information
    (let [prior-path (find-first (fn [path] (= (last path) (first move)))
                                 (inactive-paths paths))]
      (assoc paths (new-label (keys paths) move prior-path) move))))

(defn move-splits?
  [paths moves move]
  (some (fn [[det det2]] (and (= det (first move)) (not= det2 (second move))
                              (match-color? (:color det2) (:color (second move)))))
        moves))

(defn move-merges?
  [moves move]
  (some (fn [[det det2]] (and (not= det (first move)) (= det2 (second move))
                              (match-color? (:color det) (:color (first move)))))
        moves))

(defn commit-decision
  ([pdata accepted]
     (commit-decision pdata accepted []))
  ([pdata accepted alts]
     (if (empty? accepted) pdata
         (let [moves (map (fn [h] (with-meta [(:det (:data h)) (:det2 (:data h))]
                                    {:hyp h}))
                          (sort-by (comp :time :det :data)
                                   (sort-by :id accepted)))
               maxtime (dec (apply max (map :time (flatten moves))))]
           ;; incorporate the decision one time step at a time
           (loop [t (apply min (map :time (flatten moves)))
                  paths (:paths pdata)
                  log [] ;; log is reset each time
                  bad #{}]
             (if (> t maxtime)
               (assoc pdata :paths paths
                      :log log
                      :bad (map (comp :hyp meta) bad)
                      :uncovered (set/difference (:uncovered pdata)
                                                 (set (flatten (vals paths)))))
               ;; find splits and merges
               (let [moves-now (filter #(= t (:time (first %))) moves)
                     splits (filter (partial move-splits? paths moves-now)
                                    moves-now)
                     merges (filter (partial move-merges? moves-now)
                                    moves-now)

                     ;; make new labels for splits
                     ;; (new label for each second det2 of movement)
                     split-paths (reduce split-path paths splits)

                     ;; extend the paths
                     {ex-paths :paths newbad :bad newlog :log}
                     (reduce (partial extend-paths splits merges alts)
                             {:paths split-paths :bad bad :log log}
                             moves-now)
                     
                     split-merge-log
                     (concat
                      (map (fn [m] (format "Split: %s" (move-str m))) splits)
                      (map (fn [m] (format "Merge: %s" (move-str m))) merges))]
                 (recur (inc t) ex-paths
                        (vec (concat newlog split-merge-log)) newbad))))))))

(defn consistent?
  [pdata hyps alts]
  (println "checking consistency")
  (println "prior paths:" (paths-str (:paths pdata)))
  (let [t (commit-decision pdata hyps alts)]
    (println "conclusion:" (empty? (:bad t)))
    (println "log:" (:log t))
    (empty? (:bad t))))
