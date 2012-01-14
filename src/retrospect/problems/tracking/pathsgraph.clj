(ns retrospect.problems.tracking.pathsgraph
  (:require [clojure.set :as set])
  (:use [clojure.contrib.seq :only [find-first]])
  (:use [loom.graph :only
         [digraph add-edges remove-edges remove-nodes nodes edges
          incoming neighbors]])
  (:use [loom.attr :only [add-attr attr]])
  (:use [loom.io :only [view]])
  (:use [retrospect.colors])
  (:use [retrospect.problems.tracking.movements :only [valid-angle? dets-match?]])
  (:use [retrospect.state]))

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
                            (map last (vals entities)))
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
                                    (map last (vals entities))))]
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

(defn find-n-best-next
  [paths-graph det n]
  (let [scored (map (fn [det2] [det2 (:apriori (attr paths-graph det det2 :hyp))])
                    (neighbors paths-graph det))]
    (take n (map first (sort-by second scored)))))

(defn path-str
  [dets]
  (let [arrows (fn [s] (apply str (interpose " -> " s)))]
    (arrows (map (fn [det] (format "%d,%d@%d (%s)" (:x det) (:y det) (:time det)
                                   (color-str (:color det))))
                 dets))))

(defn in-corner
  [x y]
  (and (or (= x 0) (= x (dec (:GridWidth params))))
       (or (= y 0) (= y (dec (:GridHeight params))))))

(def move-fits-bias?
  (memoize
   (fn [bias [olddet det newdet]]
     (let [[x y] [(:x newdet) (:y newdet)]
           [ox oy] [(:x det) (:y det)]
           [oox ooy] [(:x olddet) (:y olddet)]]
       (or (and (in-corner x y) (in-corner ox oy))
           (valid-angle? bias x y ox oy oox ooy))))))

(defn fits-bias?
  [path bias]
  (or (>= 2 (count path))
      (every? (partial move-fits-bias? bias)
              (map (fn [par] (map #(dissoc % :time :color) par))
                   (partition 3 1 path)))))

(defn all-colors-match?
  [path]
  (let [c (find-first #(not= gray %) (map :color path))]
    (or (nil? c) (every? #(or (= gray %) (= c %)) (map :color path)))))

(defn paths-graph-paths-build
  [paths-graph paths entities entity-biases]
  (if (empty? (mapcat (fn [path] (neighbors paths-graph (last path))) paths))
    paths
    (let [path-biases (memoize
                       (fn [path-start]
                         (let [bs (filter identity
                                          (map #(:bias (get entity-biases %))
                                               (filter
                                                (fn [e]
                                                  (dets-match? path-start
                                                               (last (get entities e))))
                                                (keys entities))))]
                           (if (empty? bs) [:left :right :straight] bs))))
          new-paths (mapcat (fn [path]
                              (let [next-links
                                    (filter (fn [det]
                                              (let [p (conj path det)]
                                                (and (all-colors-match? p)
                                                     (some #(fits-bias? (take-last 3 p) %)
                                                           (path-biases (first p))))))
                                            (find-n-best-next paths-graph (last path)
                                                              (:PathBranches params)))]
                                (if (empty? next-links) [path]
                                    (map (fn [det] (conj path det)) next-links))))
                            paths)]
      (if (= (reduce + (map count paths)) (reduce + (map count new-paths)))
        new-paths
        (recur paths-graph new-paths entities entity-biases)))))

(defn paths-graph-paths
  [paths-graph entities entity-biases]
  (let [entity-ends (map last (vals entities))
        starts (filter (fn [det] (some #(dets-match? det %) entity-ends))
                       (nodes paths-graph))
        path-starts (map (fn [det] [det]) starts)
        paths (paths-graph-paths-build paths-graph path-starts entities entity-biases)
        biases [:left :right :straight]
        paths-by-bias (reduce (fn [m p]
                                (let [p-biases (filter #(fits-bias? p %) biases)]
                                  (reduce (fn [m2 b]
                                            (update-in m2 [b] conj p))
                                          m p-biases)))
                              (zipmap biases (repeat (count biases) []))
                              paths)]
    ;; turn paths into hyp sequences, per bias
    (zipmap biases
            (vec (map (fn [bias]
                        (vec (sort-by #(apply str (interpose "-" (map :id %)))
                                      (map (fn [p]
                                             (vec (map (fn [[det det2]]
                                                         (attr paths-graph det det2 :hyp))
                                                       (partition 2 1 p))))
                                           (get paths-by-bias bias)))))
                      biases)))))
