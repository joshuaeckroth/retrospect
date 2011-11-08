(ns retrospect.problems.tracking.pathsgraph
  (:require [clojure.set :as set])
  (:use [loom.graph :only
         [digraph add-edges remove-edges remove-nodes nodes edges
          incoming neighbors]])
  (:use [loom.attr :only [add-attr attr]])
  (:use [retrospect.colors])
  (:use [retrospect.problems.tracking.movements :only
         [calc-angle valid-angle?]]))

(defn paths-graph-add-edge
  [paths-graph hyp hyp-orig]
  (let [det (:det (:data hyp))
        det2 (:det2 (:data hyp))]
    (-> paths-graph
        (add-edges [det det2])
        (add-attr det :color (color-str (:color det)))
        (add-attr det :fontcolor (color-str (:color det)))
        (add-attr det :label (format "%d,%d@%d" (:x det) (:y det) (:time det)))
        (add-attr det2 :color (color-str (:color det2)))
        (add-attr det2 :fontcolor (color-str (:color det2)))
        (add-attr det2 :label (format "%d,%d@%d" (:x det2) (:y det2) (:time det2)))
        (add-attr det det2 :hyp hyp)
        (add-attr det det2 :hyp-orig hyp-orig)
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
                hyp-orig (attr paths-graph det det2 :hyp-orig)]
            (-> g (paths-graph-add-edge hnew hyp-orig))))]
    (reduce change-edge paths-graph-no-det hyp-changes)))

(defn update-paths-graph-colors
  [paths-graph entities]
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
                                  (= (:time det) (:time %)))
                            (vals entities))
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
              in-heads-possible (disj
                                 (apply set/union
                                        (if (and (empty? heads) (empty? in))
                                          #{red blue green} #{})
                                        (map #(set/union
                                               (if (not= gray (:color %)) #{(:color %)}
                                                   (if-let [p (attr g % :possible-colors)]
                                                     p #{red blue green})))
                                             (concat heads in)))
                                 gray)
              out-possible (if (or (empty? out) )
                             #{red blue green}
                             (apply set/union
                                    (map #(set/union
                                           (if (not= gray (:color %)) #{(:color %)}
                                               (if-let [p (attr g % :possible-colors)]
                                                 p #{red blue green})))
                                         out)))
              possible-colors (set/intersection in-heads-possible out-possible)
              prior-possible-colors (attr g det :possible-colors)]
          (if (or in-color out-color head-color)
            (recur (rest unchecked) (conj modified det-color)
                   (change-paths-graph-color g det det-color))
            (if (= possible-colors prior-possible-colors)
              (recur (rest unchecked) modified g)
              (recur (rest unchecked) (conj modified det)
                     (add-attr g det :possible-colors possible-colors)))))))))

(defn find-bad-edges
  [paths-graph entities]
  (let [get-heads (fn [det] (filter #(and (= (:x det) (:x %)) (= (:y det) (:y %))
                                          (= (:time det) (:time %)))
                                    (apply concat (vals entities))))]
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
  [paths-graph entities]
  (let [bad-edges (find-bad-edges paths-graph entities)]
    (apply remove-edges paths-graph bad-edges)))

(defn build-paths-graph
  [hyps entities]
  (let [pg-inconsistent (reduce (fn [g h] (paths-graph-add-edge g h h))
                                (digraph) hyps)
        pg-updated-colors (update-paths-graph-colors pg-inconsistent entities)]
    (remove-inconsistent-paths-graph-edges pg-updated-colors entities)))

(defn paths-graph-edge-hyps
  [paths-graph]
  (map (fn [[det det2]] (attr paths-graph det det2 :hyp)) (edges paths-graph)))

(defn paths-graph-paths-build
  [paths-graph paths]
  (if (empty? (mapcat (fn [path] (neighbors paths-graph (last path))) paths))
    paths
    (let [new-paths (mapcat (fn [path] (map (fn [det] (conj path det))
                                            (neighbors paths-graph (last path))))
                            paths)]
      (recur paths-graph new-paths))))

(defn valid-path?
  "Used by (paths-graph-paths); path has the form of a seq of dets."
  [path]
  (if (>= 2 (count path)) true
      (every? valid-angle?
              (map (fn [[olddet det newdet]]
                     (let [[x y] [(:x newdet) (:y newdet)]
                           [ox oy] [(:x det) (:y det)]
                           [oox ooy] [(:x olddet) (:y olddet)]]
                       (calc-angle x y ox oy oox ooy)))
                   (partition 3 1 path)))))

(defn paths-graph-paths
  [paths-graph]
  (let [starts (filter #(empty? (incoming paths-graph %)) (nodes paths-graph))
        path-starts (map (fn [det] [det]) starts)
        paths (paths-graph-paths-build paths-graph path-starts)
        valid-paths (filter valid-path? paths)]
    (map (fn [path] (map (fn [[det det2]] (attr paths-graph det det2 :hyp))
                         (partition 2 1 path)))
         valid-paths)))
