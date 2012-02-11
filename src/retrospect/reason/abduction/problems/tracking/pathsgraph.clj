(ns retrospect.reason.abduction.problems.tracking.pathsgraph
  (:require [clojure.set :as set])
  (:use [clojure.contrib.seq :only [find-first]])
  (:use [loom.graph :only
         [digraph add-edges remove-edges remove-nodes nodes edges
          incoming neighbors transpose]])
  (:use [loom.attr :only [add-attr attr]])
  (:use [loom.io :only [view]])
  (:use [retrospect.problems.tracking.colors])
  (:use [retrospect.state]))

(defn paths-graph-add-edge
  [paths-graph hyp hyp-orig]
  (let [det (:det hyp)
        det2 (:det2 hyp)]
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

(defn change-color
  [paths-graph det det-color]
  (let [in (map (fn [d] {:det d :hyp (attr paths-graph d det :hyp)})
                (incoming paths-graph det))
        out (map (fn [d] {:det d :hyp (attr paths-graph det d :hyp)})
                 (neighbors paths-graph det))
        hyp-changes (concat
                     (map (fn [{d :det h :hyp}]
                            [h (assoc h :det2 det-color)]) in)
                     (map (fn [{d :det h :hyp}]
                            [h (assoc h :det det-color)]) out))
        paths-graph-no-det (remove-nodes paths-graph det)
        change-edge
        (fn [g [h hnew]]
          (let [det (:det h)
                det2 (:det2 h)
                hyp-orig (attr paths-graph det det2 :hyp-orig)]
            (-> g (paths-graph-add-edge hnew hyp-orig))))]
    (reduce change-edge paths-graph-no-det hyp-changes)))

(defn update-colors
  [paths-graph loc-hyps]
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
                            (map :loc loc-hyps))
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
                   (change-color g det det-color))
            (if (= possible-colors prior-possible-colors)
              (recur (rest unchecked) modified g)
              (recur (rest unchecked) (conj modified det)
                     (add-attr g det :possible-colors possible-colors)))))))))

(defn find-bad-edges
  [paths-graph loc-hyps]
  (let [get-heads (fn [det] (filter #(and (= (:x det) (:x %)) (= (:y det) (:y %))
                                          (= (:time det) (:time %)))
                                    (map :loc loc-hyps)))]
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

(defn remove-inconsistent-edges
  [paths-graph loc-hyps]
  (let [bad-edges (find-bad-edges paths-graph loc-hyps)]
    (apply remove-edges paths-graph bad-edges)))

(defn build-paths-graph
  [mov-hyps loc-hyps]
  (let [pg-inconsistent (reduce (fn [g h] (paths-graph-add-edge g h h))
                                (digraph) mov-hyps)
        pg-updated-colors (update-colors pg-inconsistent loc-hyps)]
    (remove-inconsistent-edges pg-updated-colors loc-hyps)))

(defn path-str
  [dets]
  (let [arrows (fn [s] (apply str (interpose " -> " s)))]
    (arrows (map (fn [det] (format "%d,%d@%d (%s)" (:x det) (:y det) (:time det)
                                   (color-str (:color det))))
                 dets))))

(defn all-colors-match?
  [path]
  (let [c (find-first #(not= gray %) (map :color path))]
    (or (nil? c) (every? #(or (= gray %) (= c %)) (map :color path)))))

(defn paths
  [paths-graph start-det]
  (loop [ps [[start-det]]]
    (let [new-ps (mapcat (fn [path]
                           (let [next-dets (filter #(all-colors-match? (conj path %))
                                                   (neighbors paths-graph (last path)))]
                             (if (empty? next-dets) [path]
                                 (map (fn [det] (conj path det)) next-dets))))
                         ps)]
      (if (= (reduce + (map count ps)) (reduce + (map count new-ps)))
        ps (recur new-ps)))))

(defn get-paths
  [paths-graph mov-hyp]
  (let [pg-transpose (transpose paths-graph)
        paths-forward (paths paths-graph (:det2 mov-hyp))
        paths-backward (paths pg-transpose (:det mov-hyp))]
    ;; take only those paths that have at least two mov-hyps
    (filter second
            (for [b paths-backward f paths-forward]
              (map #(attr paths-graph (first %) (second %) :hyp)
                   (partition 2 1 (concat (reverse b) f)))))))