(ns retrospect.problems.abdexp.expgraph
  (:require [clojure.string :as str])
  (:use [clojure.set])
  (:use [loom.io])
  (:use [loom.graph])
  (:use [loom.alg])
  (:use [loom.attr])
  (:use [clojure.java.shell :only [sh]]))

(defn vertex?
  [expgraph vertex]
  ((nodes expgraph) vertex))

(defn values
  [expgraph vertex]
  (or (attr expgraph vertex :values) []))

(defn value
  [expgraph vertex]
  (or (attr expgraph vertex :value) "off"))

(defn vertex-value-pairs
  [expgraph]
  (apply concat (for [vertex (nodes expgraph)]
                  (for [value (values expgraph vertex)]
                    [vertex value]))))

(defn probs
  [expgraph vertex]
  (or (attr expgraph vertex :probs) {:table [] :map {}}))

(defn prob
  [expgraph vertex value parent-vals]
  (get-in (:map (probs expgraph vertex)) [(set parent-vals) value]))

(defn observation?
  [vertex]
  (= "O" (subs vertex 0 1)))

(defn forced?
  [expgraph vertex]
  (attr expgraph vertex :forced))

(defn explains
  [expgraph vertex]
  (filter #(and (nil? (attr expgraph vertex % :conflicts))
           (nil? (attr expgraph % vertex :conflicts)))
     (neighbors expgraph vertex)))

(defn explainers
  ([expgraph]
     (filter #(and (nil? (attr expgraph (first %) (second %) :conflicts))
              (nil? (attr expgraph (second %) (first %) :conflicts)))
        (edges expgraph)))
  ([expgraph vertex]
     (filter #(and (nil? (attr expgraph vertex % :conflicts))
              (nil? (attr expgraph % vertex :conflicts)))
        (incoming expgraph vertex))))

(defn unexplained?
  [expgraph vertex]
  (and (not-empty (explainers expgraph vertex))
       (not-any? (fn [v] (= "on" (value expgraph v)))
                 (explainers expgraph vertex))))

(defn forced-nodes
  [expgraph]
  (set (filter #(forced? expgraph %) (nodes expgraph))))

(defn on-nodes
  [expgraph]
  (set (filter #(= "on" (value expgraph %)) (nodes expgraph))))

(defn unexplained-nodes
  [expgraph]
  (set (filter #(unexplained? expgraph %) (on-nodes expgraph))))

(defn accepted-nodes
  [expgraph]
  (difference (on-nodes expgraph) (forced-nodes expgraph)))

(defn bottom-nodes
  [expgraph]
  (set (filter #(empty? (neighbors expgraph %)) (nodes expgraph))))

(defn top-nodes
  [expgraph]
  (set (filter #(empty? (incoming expgraph %)) (nodes expgraph))))

(defn vertices
  [expgraph]
  (nodes expgraph))

(defn data-explained-by-top
  [expgraph]
  (let [eg-filled (apply remove-nodes expgraph
                         (difference (nodes expgraph)
                                     (on-nodes expgraph)))
        explained-vs (set (mapcat #(pre-traverse eg-filled %)
                                  ;; be sure to refer back to original expgraph
                                  (top-nodes expgraph)))]
    (intersection explained-vs (forced-nodes expgraph))))

(defn turn-on
  [expgraph & vertices]
  (reduce (fn [g v] (add-attr g v :value "on")) expgraph vertices))

(defn force-on
  [expgraph & vertices]
  (reduce (fn [g v] (-> g (turn-on v) (add-attr v :forced true))) expgraph vertices))

(defn conflicts-vertices?
  [expgraph v1 v2]
  (or (attr expgraph v1 v2 :conflicts)
      (attr expgraph v2 v1 :conflicts)))

(defn conflicts?
  [expgraph [v1 val1] [v2 val2]]
  (or (= (attr expgraph v1 v2 :conflicts) [val1 val2])
      (= (attr expgraph v2 v1 :conflicts) [val2 val1])))

(defn conflicts-any?
  [expgraph v1 val1]
  (some (fn [v2] (some (fn [val2] (conflicts? expgraph [v1 val1] [v2 val2]))
                (values expgraph v2)))
     (on-nodes expgraph)))

(defn set-conflicts
  [expgraph & pairs]
  (reduce (fn [g [[v1 val1] [v2 val2]]]
       (-> g (add-edges [v1 v2])
          (add-attr v1 v2 :conflicts [val1 val2])))
     expgraph pairs))

(defn conflicts
  [expgraph]
  (map (fn [[v1 v2]]
       (let [[val1 val2] (or (attr expgraph v1 v2 :conflicts)
                             (attr expgraph v2 v1 :conflicts))]
         [[v1 val1] [v2 val2]]))
     (filter #(or (attr expgraph (first %) (second %) :conflicts)
             (attr expgraph (second %) (first %) :conflicts))
        (edges expgraph))))

(defn need-explanation
  [expgraph]
  (filter (fn [v] (and (not-empty (explainers expgraph v))
                 (not-any? #(= "on" (value expgraph %))
                           (explainers expgraph v))))
     (on-nodes expgraph)))

(defn sorted-by-dep
  ([expgraph]
     (sorted-by-dep expgraph nil))
  ([expgraph starts]
     (let [eg (let [conflicts-edges (filter #(apply conflicts-vertices? expgraph %)
                                                (edges expgraph))]
                (apply remove-edges expgraph conflicts-edges))]
       (rest (topsort (reduce (fn [g v] (add-edges g [-1 v])) (transpose eg)
                         (or starts (bottom-nodes eg)))
                      -1)))))

(defn gen-parent-combinations
  [parent-vals]
  (if (empty? parent-vals) [#{}]
      (apply concat
             (for [pv (first parent-vals)]
               (map #(conj % pv) (gen-parent-combinations (rest parent-vals)))))))

(defn format-dot-expgraph
  [expgraph true-values-map]
  (format "digraph g { node [shape=\"plaintext\"];\n %s\n %s\n %s\n }"
     ;; explains edges
     (str/join "\n" (map (fn [[v1 v2]] (format "%s -> %s;" v1 v2))
                       (explainers expgraph)))
     ;; vertices
     (str/join "\n"
               (map (fn [v]
                    (let [vals (values expgraph v)]
                      (format "%s [id=\"%s\", label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\" cellpadding=\"6\"><tr>%s</tr></table>>];"
                         v v (apply str (map (fn [val]
                                             (format "<td port=\"%s%s\" bgcolor=\"%s\">%s=%s</td>"
                                                v val
                                                (if (= val (true-values-map v))
                                                  "#eeeeee" "#ffffff")
                                                v val))
                                           vals)))))
                  (vertices expgraph)))
     ;; conflicts edges
     (str/join "\n" (map (fn [[[v1 val1] [v2 val2]]]
                         (format "%s:%s%s -> %s:%s%s [dir=\"none\", style=\"dotted\", constraint=false];"
                            v1 v1 val1 v2 v2 val2))
                       (conflicts expgraph)))))

(defn gen-vertex-graph-positions
  [expgraph]
  (let [expgraph-no-conflicts (reduce (fn [eg [[v1 _] [v2 _]]] (remove-edges eg [v1 v2]))
                                 expgraph (conflicts expgraph))
        expgraph-conflicts-nodes (reduce (fn [eg [[v1 val1] [v2 val2]]]
                                      (let [c-node (format "%s_%s_C_%s_%s"
                                                      v1 val1 v2 val2)]
                                        (-> eg
                                           (add-attr c-node :id c-node)
                                           (add-attr c-node :label c-node)
                                           (add-edges [v1 c-node] [v2 c-node]))))
                                    expgraph-no-conflicts (conflicts expgraph))
        dot (dot-str expgraph-conflicts-nodes
                     :node {:width 2 :height 1 :fixedsize true})
        {cmapx :out} (sh "dot" "-Tcmapx_np" "-NURL=a" :in dot)]
    (reduce (fn [m [_ v x1 y1 x2 y2]]
         (assoc m v [(/ (+ (Double/parseDouble x1) (Double/parseDouble x2)) 2.0)
                     (/ (+ (Double/parseDouble y1) (Double/parseDouble y2)) 2.0)]))
       {} (re-seq #"id=\"(.*?)\".*coords=\"(\d+),(\d+),(\d+),(\d+)" cmapx))))
