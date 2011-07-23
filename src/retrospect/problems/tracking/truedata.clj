(ns retrospect.problems.tracking.truedata
  (:use [retrospect.random])
  (:use [retrospect.colors])
  (:use [retrospect.problems.tracking.grid])
  (:use [clojure.contrib.seq :only [find-first]])
  (:require [clojure.set :as set]))

(defn add-new-entities
  [grid numes]
  (reduce (fn [g _] (grid-new-entity g 0)) grid (range numes)))

(defn random-walks
  [grid params]
  (let [maxwalk (:MaxWalk params)
        es (grid-entities grid)
        es-walk (my-shuffle
                 (flatten (map (fn [e] (repeat (inc (my-rand-int maxwalk)) e)) es)))]
    (reduce walk1 grid es-walk)))

(defn possibly-add-new-entity
  [grid time params]
  (if (>= (double (/ (:ProbNewEntities params) 100)) (my-rand))
    (grid-new-entity grid time)
    grid))

(defn output-walk-sizes
  [truedata params]
  (let [dists (mapcat (fn [grid] (map #(dist (:ox %) (:oy %) (:x %) (:y %))
                                      (vals (:movements (meta grid)))))
                      (rest truedata))
        dist-counts (reduce (fn [dc d] (if (dc (str d))
                                         (update-in dc [(str d)] inc)
                                         (assoc dc (str d) 1))) {} dists)
        dc-strs (map #(format "%s,%d" % (dist-counts %)) (sort (keys dist-counts)))]
    (spit "walks.txt" (str (count dists) "\n"
                           (apply str (interpose "\n" dc-strs))))))

(defn generate-truedata
  [datadir params]
  (let [grid (add-new-entities (new-grid (:GridWidth params) (:GridHeight params))
                               (:NumberEntities params))]
    (loop [time 1
           truedata [grid]]
      (if (> time (:Steps params))
        ;(do (output-walk-sizes truedata params) truedata) 
        truedata
        (let [newgrid (-> (last truedata)
                        (update-all-entities time)
                        (random-walks params)
                        (possibly-add-new-entity time params))]
          (recur (inc time) (conj truedata newgrid)))))))

(defn get-grid-movements
  [truedata mintime maxtime]
  (if (< maxtime 0) []
      (mapcat (fn [t] (vals (:movements (meta (nth truedata (inc t))))))
              (range mintime (inc maxtime)))))

(defn true-movements
  [truedata maxtime]
  (set (map #(dissoc % :e) (get-grid-movements truedata 0 maxtime))))

(defn get-entity-movements
  [truedata mintime maxtime believed-moves]
  (let [moves (get-grid-movements truedata mintime maxtime)
        true-moves (true-movements truedata maxtime)
        entities (sort (set (map :e moves)))
        arrows (fn [ss] (apply str (interpose " -> " ss)))
        entity-pos-list
        (fn [e] (arrows (concat
                         (let [m (find-first #(= e (:e %)) moves)]
                           [(format "%d,%d@%d" (:ox m) (:oy m) (:ot m))])
                         (for [m moves :when (= (:e m) e)]
                           (if (some #{(dissoc m :e)} believed-moves)
                             (format "%d,%d@%d" (:x m) (:y m) (:t m))
                             (format "! %d,%d@%d" (:x m) (:y m) (:t m)))))))]
    (apply str (map (fn [e] (format "%s (%s): %s\n"
                                    (str e) (color-str (:color (meta e)))
                                    (entity-pos-list e)))
                    entities))))


