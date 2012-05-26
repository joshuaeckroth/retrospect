(ns retrospect.problems.tracking.truedata
  (:import (misc AlphanumComparator))
  (:use [retrospect.random])
  (:use [retrospect.problems.tracking.colors])
  (:use [retrospect.problems.tracking.movements])
  (:use [retrospect.state]))

(defn add-new-entities
  [movements numes]
  (reduce (fn [m _] (new-entity m 0)) movements (range numes)))

(defn random-walks
  [movements time]
  (let [maxwalk (:MaxWalk params)]
    (reduce #(walk %1 %2 time maxwalk) movements (my-shuffle (entities movements)))))

(defn output-walk-sizes
  [movements]
  (let [dists (map #(dist (:ox %) (:oy %) (:x %) (:y %))
                   (mapcat rest (vals movements)))
        dist-counts (reduce (fn [dc d]
                              (if (dc (str d))
                                (update-in dc [(str d)] inc)
                                (assoc dc (str d) 1)))
                            {} dists)
        dc-strs (map #(format "%s,%d\n" % (get dist-counts %))
                     (sort (keys dist-counts)))]
    (spit (format "walks-%s.txt" (:MaxWalk params))
          (str (count dists) "\n" (apply str dc-strs)))))

(defn generate-truedata
  []
  (let [movements (add-new-entities
                   (new-movements (:GridWidth params) (:GridHeight params))
                   (:NumberEntities params))]
    (loop [time 1
           m movements]
      (if (> time (:Steps params))
        ;; todo: fix (don't train on same stuff as test)
        (do (comment (output-walk-sizes m)) {:test m :training {:test m}})
        (recur (inc time) (random-walks m time))))))

(defn format-movements-comparative
  [true-movements believed-movements mintime maxtime]
  (let [es (sort-by str (AlphanumComparator.) (entities true-movements))
        arrows (fn [ss] (apply str (interpose " -> " ss)))
        lines (fn [ss] (apply str (interpose "\n" ss)))]
    (lines (for [e es]
             (format "%s (%s): %s"
                     e (color-str (:color (first (get true-movements e))))
                     (arrows (for [{:keys [x y time] :as mov}
                                   (entity-movements true-movements e mintime maxtime)]
                               (if (or (= 0 time)
                                       (some #(moves-match? mov %)
                                             believed-movements))
                                 (format "%d,%d@%d" x y time)
                                 (format "!! %d,%d@%d" x y time)))))))))
