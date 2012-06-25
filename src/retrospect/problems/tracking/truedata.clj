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

(defn generate-movements
  [max-time]
  (let [movements (add-new-entities
                   (new-movements (:GridWidth params) (:GridHeight params))
                   (:NumberEntities params))]
    (loop [time 1
           m movements]
      (if (> time max-time) m
          (recur (inc time) (random-walks m time))))))

(defn generate-truedata
  []
  (let [test-movements (generate-movements (:Steps params))
        training-steps (int (* (/ (double (:Knowledge params)) 100.0)
                               (:TrainingSteps params)))
        training-movements (generate-movements training-steps)]
    {:test test-movements
     :all-moves (set (apply concat (vals test-movements)))
     :training {:test training-movements
                :all-moves (set (apply concat (vals training-movements)))}}))

(defn format-movements-comparative
  [true-movements believed-movements mintime maxtime]
  (let [es (sort-by str (AlphanumComparator.) (entities true-movements))
        arrows (fn [ss] (apply str (interpose " -> " ss)))
        lines (fn [ss] (apply str (interpose "\n" ss)))
        bel-movs-set (set believed-movements)]
    (lines (for [e es]
             (format "%s (%s): %s"
                e (color-str (:color (first (get true-movements e))))
                (arrows (for [{:keys [x y time] :as mov}
                              (entity-movements true-movements e mintime maxtime)]
                          (if (or (= 0 time) (bel-movs-set mov))
                            (format "%d,%d@%d" x y time)
                            (format "!! %d,%d@%d" x y time)))))))))
