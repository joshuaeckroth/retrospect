(ns retrospect.problems.tracking.hypotheses
  (:use [retrospect.epistemicstates :only [add-hyp add-more-hyp add-fact]])
  (:use [retrospect.workspaces :only [new-hyp]])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.colors])
  (:use [retrospect.confidences])
  (:use [retrospect.problems.tracking.grid :only [grid-at dist]])
  (:use [clojure.contrib.seq :only [find-first]])
  (:require [clojure.contrib.math :as math])
  (:require [clojure.set :as set :only [intersection difference]])
  (:require [loom.io :as loom.io])
  (:use [loom.graph :only
         [digraph add-edges remove-edges remove-nodes nodes edges
          incoming neighbors]])
  (:use [loom.attr :only [add-attr attr]]))

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

(defn make-sensor-hyps
  [sensor e]
  (let [desc (format (str "Sensor detection by %s - color: %s, "
                          "x: %d, y: %d, time: %d")
                     (:id sensor) (color-str (:color (meta e)))
                     (:x (meta e)) (:y (meta e)) (:time (meta e)))]
    ;; return an 'entity' with hyp data in the meta map
    (with-meta e (merge (meta e)
                        (zipmap [:hyp-from :hyp-to]
                                (map #(new-hyp % :sensor nil NEUTRAL desc
                                               {:sensor sensor :entity e})
                                     ;; sensor hyp 'from', sensor hyp 'to'
                                     ["SHF" "SHT"]))))))

(defn sensors-to-spotted
  "Pull the sensor detections between mintime and maxtime from each
   sensor and incorporate into a collection of grids (one grid for each
   time step)."
  [sensors mintime maxtime sensors-seen-grid]
  (let [width (:width (meta sensors-seen-grid))
        height (:height (meta sensors-seen-grid))
        ;; make entities/hyps of all detections between mintime and maxtime
        ;; from all sensors
        es (mapcat (fn [t] (mapcat (fn [s] (map #(make-sensor-hyps s %)
                                                (sensed-at s t)))
                                   sensors))
                   (range mintime (inc maxtime)))
        ;; create blank grids, one for each time
        spotted-grids (vec (map #(with-meta
                                   (vec (repeat (* width height) []))
                                   {:width width :height height :time %})
                                (range mintime (inc maxtime))))
        ;; helper function to put a sensor hyp in appropriate grid
        put-in-grid (fn [sgs e]
                      (let [t (:time (meta e))
                            x (:x (meta e))
                            y (:y (meta e))
                            sg (get sgs (- t mintime))]
                        (assoc sgs (- t mintime)
                               (update-in sg [(+ x (* y width))]
                                          conj e))))]
    [(reduce put-in-grid spotted-grids es) es]))

(defn process-sensors
  "For each time step between the last time we processed sensor data
   and the current time, look at the sensor detections at that time
   step, incorporate them into our running estimate of the grid, add
   those detections as 'facts,' and record those detections as
   'uncovered.'"
  [ep-state sensors time-now]
  (let [sg (:spotted-grid (:problem-data ep-state))
        mintime (count sg)
        sensors-seen-grid (:sensors-seen-grid (:problem-data ep-state))
        [sg-new sg-new-flat] (sensors-to-spotted sensors mintime time-now
                                                 sensors-seen-grid)
        ;; get all the from/to hyps; ignore 'from' hyps if time is 0
        hyps-to-add (concat
                     (map (comp :hyp-to meta)
                          (concat (filter #(not= time-now (:time (meta %)))
                                          sg-new-flat)
                                  (flatten (or (last sg) []))))
                     (filter #(not= 0 (:time (meta (:entity (:data %)))))
                             (map (comp :hyp-from meta) sg-new-flat)))]
    (reduce
     #(add-fact %1 %2 [])
     (-> ep-state
         (assoc-in [:problem-data :uncovered]
                   (set (concat (:uncovered (:problem-data ep-state))
                                (map #(select-keys (meta %) [:x :y :time :color])
                                     sg-new-flat))))
         (update-in [:problem-data :spotted-grid] concat sg-new))
     hyps-to-add)))

(defn get-path-heads
  [paths]
  (map (fn [l] (assoc (last (paths l)) :color (:color (meta l)))) (keys paths)))

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
  [det uncovered spotted-grid entity-hyps params]
  (let [find-spotted (fn [d] (filter #(match-color? (:color d) (:color (meta %)))
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
                (let [e-hyp (find-first
                             #(let [d (:det (:data %))
                                    e (:entity (:data %))]
                                (and (= (:x d) (:x det))
                                     (= (:y d) (:y det))
                                     (= (:time d) (:time det))
                                     (match-color? (:color (meta e))
                                                   (:color det))))
                             entity-hyps)
                      explains (concat (map (comp :hyp-from meta)
                                            (find-spotted det2))
                                       (map (comp :hyp-to meta)
                                            (find-spotted det))
                                       (if e-hyp [e-hyp] []))]
                  [[(new-hyp "TH" :tracking :shared-explains
                             score (desc-fn det det2 explains)
                             {:det det :det2 det2})
                    explains]
                   ;; split-merge hyp
                   [(new-hyp "THSM" :tracking nil
                             (penalize score) (format "%s\n\n(split-merge)"
                                           (desc-fn det det2 explains))
                             {:det det :det2 det2})
                    explains]]))))))

(defn make-known-entities-hyps
  [paths time-now steps-between]
  (let [earliest-time (- time-now steps-between)
        hyp-to (fn [l t] (new-hyp "TE" :tracking-entity nil NEUTRAL
                                  (format "Where did %s go at %d?" l t)
                                  {:det (last (paths l)) :entity l}))]
    (map #(hyp-to % (:time (last (paths %))))
         (filter #(and (not (:dead (meta %)))
                       (<= earliest-time (:time (last (paths %)))))
                 (keys paths)))))

(defn paths-graph-add-edge
  [paths-graph hyp hyp-orig explains]
  (let [det (:det (:data hyp))
        det2 (:det2 (:data hyp))]
    (-> paths-graph
        (add-edges [det det2])
        (add-attr det :color (color-str (:color det)))
        (add-attr det :fontcolor (color-str (:color det)))
        (add-attr det :label (format "%d,%d@%d"
                                     (:x det) (:y det) (:time det)))
        (add-attr det2 :color (color-str (:color det2)))
        (add-attr det2 :fontcolor (color-str (:color det2)))
        (add-attr det2 :label (format "%d,%d@%d"
                                      (:x det2) (:y det2) (:time det2)))
        (add-attr det det2 :hyp hyp)
        (add-attr det det2 :hyp-orig hyp-orig)
        (add-attr det det2 :explains explains)
        (add-attr det det2 :label (:id hyp)))))

(defn change-paths-graph-color
  [paths-graph det det-color]
  (let [in (map (fn [d] {:det d :hyp (attr paths-graph d det :hyp)})
                (incoming paths-graph det))
        out (map (fn [d] {:det d :hyp (attr paths-graph det d :hyp)})
                 (neighbors paths-graph det))
        hyp-changes (concat
                     (map (fn [{d :det h :hyp}]
                            [h (assoc-in h [:data :det2] det-color)]) in)
                     (map (fn [{d :det h :hyp}]
                            [h (assoc-in h [:data :det] det-color)]) out))
        paths-graph-no-det (remove-nodes paths-graph det)
        change-edge
        (fn [g [h hnew]]
          (let [det (:det (:data h))
                det2 (:det2 (:data h))
                explains (attr paths-graph det det2 :explains)
                hyp-orig (attr paths-graph det det2 :hyp-orig)]
            (-> g (paths-graph-add-edge hnew hyp-orig explains))))]
    (reduce change-edge paths-graph-no-det hyp-changes)))

(defn update-paths-graph-colors
  [paths-graph path-heads]
  (let [grays #(filter (fn [det] (= gray (:color det))) %)]
    (loop [unchecked (grays (nodes paths-graph))
           modified #{}
           g paths-graph]
      (if (empty? unchecked)
        (if (empty? modified) g
            (recur (grays (mapcat #(concat (incoming g %) (neighbors g %))
                                  modified))
                   #{} g))
        (let [det (first unchecked)
              heads (filter #(and (= (:x det) (:x %)) (= (:y det) (:y %))
                                  (= (:time det) (:time %))) path-heads)
              in (incoming g det)
              out (neighbors g det)
              count-color (fn [dets color]
                            (count (filter #(= color (:color %)) dets)))
              single-color (fn [dets]
                             (let [c-red (count-color dets red)
                                   c-blue (count-color dets blue)
                                   c-green (count-color dets green)
                                   c-gray (count-color dets gray)]
                               (if (= 0 c-gray)
                                 (cond
                                  (and (= 0 c-blue)
                                       (= 0 c-green)
                                       (not= 0 c-red))
                                  red
                                  (and (= 0 c-red)
                                       (= 0 c-green)
                                       (not= 0 c-blue))
                                  blue
                                  (and (= 0 c-red)
                                       (= 0 c-blue)
                                       (not= 0 c-green))
                                  green))))
              head-color (single-color heads)
              in-color (single-color in)
              out-color (single-color out)
              det-color (assoc det :color (or in-color out-color head-color))
              possible-colors
              (apply set/intersection #{blue green red}
                     (filter not-empty
                             (map (fn [dets]
                                    (set (filter #(not= 0 (count-color dets %))
                                                 [blue red green])))
                                  [heads in out])))]
          (if (or in-color out-color head-color)
            (recur (rest unchecked) (conj modified det-color)
                   (change-paths-graph-color g det det-color))
            (recur (rest unchecked) modified
                   (add-attr g det :possible-colors possible-colors))))))))

(defn find-bad-edges
  [paths-graph path-heads]
  (let [get-heads
        (fn [det] (filter #(and (= (:x det) (:x %)) (= (:y det) (:y %))
                                (= (:time det) (:time %))) path-heads))]
    (filter
     (fn [[det det2]]
       (let [heads-det (get-heads det)
             heads-det2 (get-heads det2)
             possible-match?
             (fn [det det-other]
               (let [pc (attr paths-graph det :possible-colors)]
                 (if (not-empty pc)
                   (some #(match-color? (:color det-other) %) pc)
                   (match-color? (:color det) (:color det-other)))))]
         (or
          (and (not-empty heads-det)
               (every? #(not (possible-match? det %)) heads-det))
          (and (not-empty heads-det2)
               (every? #(not (possible-match? det2 %)) heads-det2))
          (not (possible-match? det det2))
          (not (possible-match? det2 det)))))
     (edges paths-graph))))

(defn remove-inconsistent-paths-graph-edges
  [paths-graph path-heads]
  (let [bad-edges (find-bad-edges paths-graph path-heads)]
    {:paths-graph (apply remove-edges paths-graph bad-edges)
     :count-removed (count bad-edges)}))

(defn inconsistent
  [pdata hyps rejected]
  (let [relevant-hyps (filter #(= :tracking (:type %)) hyps)
        path-heads (get-path-heads (:paths pdata))
        paths-graph (:paths-graph pdata)
        hyp-to-edge (fn [h] [(:det (:data h)) (:det2 (:data h))])
        removable-edges (set/union (set (map hyp-to-edge rejected))
                                   (set/difference
                                    (set (edges paths-graph))
                                    (set (map hyp-to-edge relevant-hyps))))
        pg-clean (apply remove-edges paths-graph removable-edges)
        pg-colors (update-paths-graph-colors pg-clean path-heads)
        bad-edges (find-bad-edges pg-colors path-heads)]
    (map (fn [[det det2]] (attr pg-colors det det2 :hyp-orig)) bad-edges)))

(defn build-paths-graph
  [paths-graph hyps path-heads]
  (let [pg-inconsistent (reduce (fn [g [h e]] (paths-graph-add-edge g h h e))
                                paths-graph hyps)
        pg-updated-colors (update-paths-graph-colors pg-inconsistent path-heads)]
    (remove-inconsistent-paths-graph-edges pg-updated-colors path-heads)))

(defn hypothesize
  "Process sensor reports, then make hypotheses for all possible movements,
   and add them to the epistemic state."
  [ep-state sensors time-now params]
  (let [paths (:paths (:problem-data ep-state))
        path-heads (get-path-heads paths)
        entity-hyps (make-known-entities-hyps
                     paths time-now (:StepsBetween params))
        ep-entities (reduce #(add-fact %1 %2 []) ep-state entity-hyps)
        ep (process-sensors ep-entities sensors time-now)
        sg (:spotted-grid (:problem-data ep))
        uncovered (set/union (set path-heads) (:uncovered (:problem-data ep)))]
    (loop [hyps []
           split-merge-hyps []
           unc uncovered]
      (if (empty? unc)
        ;; ran out of uncovered detections; so add all the hyps
        (let [{:keys [paths-graph count-removed]}
              (build-paths-graph (digraph) hyps path-heads)
              pdata (assoc (:problem-data ep)
                      :paths-graph paths-graph
                      :count-removed count-removed
                      :split-merge-hyps split-merge-hyps)
              ep-paths-graph (assoc ep :problem-data pdata)
              consistent-hyps (map (fn [[det det2]]
                                     [(attr paths-graph det det2 :hyp)
                                      (attr paths-graph det det2 :explains)])
                                   (edges paths-graph))]
          (reduce (fn [ep [hyp explains]] (add-hyp ep hyp explains))
                  ep-paths-graph consistent-hyps))
        ;; take the first uncovered detection, and make movement hyps out of it
        (let [mov-hyps (make-movement-hyps (first unc) uncovered
                                           sg entity-hyps params)]
          (recur (concat hyps (map first mov-hyps))
                 (concat split-merge-hyps (map second mov-hyps))
                 (rest unc)))))))

(defn get-more-hyps
  [ep-state]
  (let [hyps (:split-merge-hyps (:problem-data ep-state))
        paths (:paths (:problem-data ep-state))
        path-heads (get-path-heads paths)
        paths-graph (:paths-graph (:problem-data ep-state))
        {new-paths-graph :paths-graph count-removed :count-removed}
        (build-paths-graph paths-graph hyps path-heads)
        ep (-> ep-state
               (assoc-in [:problem-data :split-merge-hyps] [])
               (assoc-in [:problem-data :paths-graph] new-paths-graph)
               (update-in [:problem-data :count-removed] + count-removed))]
    (reduce (fn [ep [hyp explains]] (add-more-hyp ep hyp explains)) ep hyps)))

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
  color, than obviously the new color should be gray."
  ([path]
     (let [colors (map :color path)
           has-red? (some #{red} colors)
           has-blue? (some #{blue} colors)
           has-green? (some #{green} colors)]
       (cond (and (not has-red?) (not has-blue?) (not has-green?)) gray
             (and has-red? (not has-blue?) (not has-green?)) red
             (and has-blue? (not has-red?) (not has-green?)) blue
             (and has-green? (not has-red?) (not has-blue?)) green
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
    (cond
     ;; if we have a split, just mark each applicable label as dead
     (some #{move} splits)
     (if (not-empty before-labels)
       {:paths (reduce (fn [ps l]
                         (-> ps (dissoc l)
                             (assoc (with-meta l (merge (meta l) {:dead true}))
                               (get ps l))))
                       paths before-labels)
        :log (conj log (format
                        "%s is a split of %s." (move-str move)
                        (apply str (interpose "," (map str before-labels)))))
        :bad bad}
       ;; no label found, return paths
       {:paths paths
        :log (conj log (format "%s is a split but no label found."
                               (move-str move)))
        :bad bad})
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
     ;; this movement's second det is not gray (because if it was,
     ;; basically anything's possible because nothing can be said),
     ;; this movement continues no path, yet there is a label meeting
     ;; the point of the movement; additionally, there are no possible
     ;; alternative explainers for this move's head or tail; in such
     ;; cases, the colors don't match; make a new label, but mark the
     ;; movement as bad
     (and (empty? alts-for-move)
          (not= gray (:color (second move)))
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

