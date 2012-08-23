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
  [movements time mean variance]
  (reduce #(walk %1 %2 time mean variance) movements (my-shuffle (entities movements))))

(defn generate-movements
  [mean variance steps]
  (let [movements (add-new-entities
                   (new-movements (:GridWidth params) (:GridHeight params))
                   (:NumberEntities params))]
    (loop [time 1
           m movements]
      (if (> time steps) m
          (recur (inc time) (random-walks m time mean variance))))))

(defn generate-truedata
  []
  (let [test-movements (generate-movements (:TrueMoveMean params)
                                           (:TrueMoveVariance params)
                                           (:Steps params))
        training-movements (generate-movements (:BelMoveMean params)
                                               (:BelMoveVariance params)
                                               (:TrainingSteps params))]
    {:test test-movements
     :all-moves (set (filter :ot (apply concat (vals test-movements))))
     :training {:moves (filter :ot (apply concat (vals training-movements)))
                ;; give all true seen colors (for now)
                :seen-colors (set (map :color (apply concat (vals test-movements))))}}))

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
