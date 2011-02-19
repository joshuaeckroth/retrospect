(ns samre.problems.tracking.hypotheses
  (:use [samre.epistemicstates :only [add-hyp add-fact]])
  (:use [samre.workspaces :only [new-hyp]])
  (:use [samre.sensors :only [sensed-at]])
  (:use [samre.colors])
  (:use [samre.confidences])
  (:use [samre.problems.tracking.grid :only [grid-at]])
  (:require [clojure.contrib.math :as math :only [abs ceil]])
  (:require [clojure.set :as set :only [intersection]]))

(defn add-sensor-hyp
  [sensor e]
  (let [hyp (new-hyp "SH" :sensor NEUTRAL [] (constantly []) (constantly [])
                     (fn [h] (format "%s" (str (meta (:entity (:data h))))))
                     {:sensor sensor :entity e})]
    (with-meta e (merge (meta e) {:hyp hyp}))))

(defn sensors-to-spotted
  [sensors time sensors-seen-grid]
  (let [width (:width (meta sensors-seen-grid))
        height (:height (meta sensors-seen-grid))]
    (with-meta
      (for [y (range height) x (range width)] ;; need height followed by width
        (apply concat (map (fn [s]
                             (map #(add-sensor-hyp s %)
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
                 (reduce add-fact ep (map (comp :hyp meta) (flatten spotted)))))))))

(defn man-dist
  [x1 y1 x2 y2]
  (+ (math/abs (- x1 x2)) (math/abs (- y1 y2))))

(defn score-distance
  [x1 y1 x2 y2 maxwalk]
  (let [dist (man-dist x1 y1 x2 y2)]
    (cond
     (<= dist (math/ceil (/ maxwalk 4))) VERY-PLAUSIBLE
     (<= dist (math/ceil (/ maxwalk 2))) PLAUSIBLE
     ;; last case is same as :else due to physical constraints
     (<= dist maxwalk) NEUTRAL))) 

(defn score-path
  "spotted is a collection of sensor detections; count-seen is how
  many sensors should have seen the same thing."
  [path label maxwalk]
  (let [move-pairs (partition 2 (interleave path (rest path)))]
    (if (empty? move-pairs) NEUTRAL
        (apply min (map (fn [[a b]] (score-distance (:x (meta (first a)))
                                                    (:y (meta (first a)))
                                                    (:x (meta (first b)))
                                                    (:y (meta (first b)))
                                                    maxwalk))
                        move-pairs)))))

(defn path-str
  [path]
  (let [fmt #(format "%d,%d@%d " (:x (meta %)) (:y (meta %)) (:time (meta %)))]
    (str "[" (apply str (map (fn [es] (fmt (first es))) path)) "]")))

(defn print-paths
  [paths]
  (println "paths " (apply str (map (fn [k] (apply str k ": " (map path-str (k paths))))
                                    (keys paths)))))

(defn covered?
  [paths x y time]
  (some (fn [e] (and (= (:time (meta e)) time)
                     (= (:x (meta e)) x)
                     (= (:y (meta e)) y)))
        (flatten paths)))

(defn find-uncovered-pos
  "Finds the earliest spotted positions that have not been covered."
  [paths spotted-grid]
  (if (empty? spotted-grid) []
      (let [grid (first spotted-grid)
            time (:time (meta grid))
            uncovered
            (filter identity
                    (for [y (range (:height (meta grid))) ;; need height followed by width
                          x (range (:width (meta grid)))]
                      (if (and (not-empty (grid-at grid x y))
                               (not (covered? (vals paths) x y time)))
                        {:x x :y y :time time})))]
        (if (not-empty uncovered) uncovered
            (recur paths (rest spotted-grid))))))

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
       (>= maxwalk (man-dist x y (:x (meta e))
                             (:y (meta e))))))

(defn spotted-in-range
  [label path prior-covered spotted-grid maxwalk]
  (let [{x :x y :y t :time} (meta (first (last path)))
        grid (first spotted-grid)]
    (filter (fn [es] (some #(matched-and-in-range? label % x y t prior-covered maxwalk)
                           es)) grid)))

(defn extend-path
  [label path prior-covered spotted-grid maxwalk]
  (let [in-range (spotted-in-range label path prior-covered spotted-grid maxwalk)]
    (map (fn [e] (conj path e)) in-range)))

(defn extend-paths
  [label paths prior-covered spotted-grid maxwalk]
  (apply concat paths (map (fn [p] (extend-path label p prior-covered spotted-grid maxwalk))
                           paths)))

(defn make-label-path
  "The first grid in spotted-grid is the time step that should be
  connected; on the recur, strip off the first grid; if spotted-grid
  has no more grids, we're done."
  [label paths prior-covered spotted-grid maxwalk]
  (if (empty? spotted-grid) paths
      (let [ex-paths (extend-paths label paths prior-covered spotted-grid maxwalk)]
        ;; check for count=1 (rather than 0) because existing path is always included
        (if (= 1 (count ex-paths)) paths
            (recur label ex-paths prior-covered (rest spotted-grid) maxwalk)))))

(defn assoc-label-path
  [label paths prior-covered spotted-grid maxwalk]
  (let [prior-paths (label paths)
        last-time (apply min (map (comp :time meta first last) prior-paths))
        sg (if-not prior-paths spotted-grid (subvec spotted-grid (inc last-time)))]
    (assoc paths label (make-label-path label prior-paths prior-covered sg maxwalk))))

(defn str-fn
  [hyp]
  (format "%s" (path-str (:path (:data hyp)))))

(defn impossible-fn
  [hyp hyps]
  (let [explains (set (:explains hyp))]
    (filter (fn [h] (and (not= (:id hyp) (:id h))
                         (not-empty (set/intersection explains (set (:explains h))))))
            hyps)))

(defn make-hyp
  [path label maxwalk]
  (new-hyp "TH" :tracking (score-path path label maxwalk)
           (map :id (filter identity (map (comp :hyp meta) (flatten path))))
           (constantly []) impossible-fn
           str-fn {:label label :path path}))

(defn hypothesize
  [ep-state sensors time-now params]
  (let [ep (process-sensors ep-state sensors time-now)
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
      (let [uncovered (find-uncovered-pos paths spotted-grid)]
        (if (empty? uncovered)
          (do
            (println (map (fn [l] (format "%s (%s)" (str l) (str (meta l)))) (keys paths)))
            (print-paths paths)
            (reduce add-hyp ep (apply concat (map (fn [l] (map #(make-hyp % l maxwalk)
                                                               (l paths)))
                                                  (keys paths)))))
          (let [label (new-label (keys paths) (spotted-at (first uncovered)))
                newpaths (assoc paths label [[(spotted-at (first uncovered))]])]
            (recur (assoc-label-path label newpaths prior-covered spotted-grid maxwalk))))))))

;; TODO: check for ambiguity (unexplained), make new label for each alternative

(defn commit-rejected
  [pdata rejected]
  (reduce (fn [pd hyp] (let [l (:label (:data hyp))]
                         (update-in pd [:paths] dissoc l)))
          pdata rejected))

(defn commit-accepted
  [pdata accepted]
  (reduce (fn [pd hyp] (let [l (:label (:data hyp)) path (:path (:data hyp))]
                         (update-in pd [:paths] assoc l path)))
          pdata accepted))

(defn commit-decision
  "Commit rejected first, then accepted."
  [pdata accepted rejected candidates]
  #_(println (map :pid candidates))
  #_(println (map (comp path-str :path :data) candidates))
  (commit-accepted (commit-rejected pdata rejected) accepted))

