(ns retrospect.problems.tracking.truedata
  (:use [retrospect.random])
  (:use [retrospect.colors])
  (:use [retrospect.problems.tracking.movements])
  (:use [retrospect.state]))

(defn add-new-entities
  [movements numes]
  (reduce (fn [m _] (new-entity m 0)) movements (range numes)))

(defn random-walks
  [movements time]
  (let [maxwalk (:MaxWalk params)]
    (reduce #(walk %1 %2 time maxwalk) movements (entities movements))))

(defn possibly-add-new-entity
  [movements time]
  (if (>= (double (/ (:ProbNewEntities params) 100)) (my-rand))
    (new-entity movements time)
    movements))

;; TODO: fix for 'movements' structure
(defn output-walk-sizes
  [movements]
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
  []
  (let [movements (add-new-entities
                   (new-movements (:GridWidth params) (:GridHeight params))
                   (:NumberEntities params))]
    (loop [time 1
           m movements]
      (if (>= time (:Steps params)) m
        ;(do (output-walk-sizes m params) m) 
        (recur (inc time) (-> m (random-walks time)
                              (possibly-add-new-entity time)))))))

(defn format-movements-comparative
  [true-movements believed-movements mintime maxtime]
  (let [es (sort (entities true-movements))
        all-believed (set (apply concat (vals believed-movements)))
        arrows (fn [ss] (apply str (interpose " -> " ss)))
        lines (fn [ss] (apply str (interpose "\n" ss)))]
    (lines (for [e es] (format "%s (%s): %s"
                               e (color-str (:color (first (get true-movements e))))
                               (arrows (for [{:keys [x y time] :as mov}
                                             (entity-movements true-movements e mintime maxtime)]
                                         (if (all-believed mov)
                                           (format "%d,%d@%d" x y time)
                                           (format "!! %d,%d@%d" x y time)))))))))
