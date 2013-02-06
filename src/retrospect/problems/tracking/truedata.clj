(ns retrospect.problems.tracking.truedata
  (:import (misc AlphanumComparator))
  (:use [retrospect.random])
  (:use [retrospect.problems.tracking.colors])
  (:use [retrospect.problems.tracking.movements])
  (:use [retrospect.state]))

(defn add-new-entities
  [movements numes]
  (reduce (fn [m _] (new-entity m 0)) movements (range numes)))

(defn generate-truedata
  []
  (let [test-starting-movements (add-new-entities
                                 (new-movements (:GridWidth params) (:GridHeight params))
                                 (:NumberEntities params))
        test-movements (generate-movements
                        test-starting-movements
                        (:Steps params) true false)
        testing-movs (apply concat (vals test-movements))
        training-starting-r (add-new-entities
                             (new-movements (:GridWidth params) (:GridHeight params))
                             (:NumberEntities params))
        training-starting-t (add-new-entities
                             (new-movements (:GridWidth params) (:GridHeight params))
                             (:NumberEntities params))
        training-movs (filter :ot (concat (apply concat
                                            (vals (generate-movements
                                                   training-starting-r
                                                   (:TrainingRandom params)
                                                   false true)))
                                     (apply concat
                                            (vals (generate-movements
                                                   training-starting-t
                                                   (:TrainingSteps params)
                                                   false false)))))]
    {:test test-movements
     :all-moves (set (filter :ot testing-movs))
     :all-xys (set (concat (map (fn [mov] {:x (:x mov) :y (:y mov) :time (:time mov)})
                              testing-movs)
                           (map (fn [mov] {:x (:ox mov) :y (:oy mov) :time (:ot mov)})
                              testing-movs)))
     :training {:moves training-movs
                ;; give all true seen colors (for now)
                :seen-colors (set (map :color testing-movs))}}))

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
                          (if (or (= 0 time) (some #(moves-match? mov %) believed-movements))
                            (format "%d,%d@%d" x y time)
                            (format "!! %d,%d@%d" x y time)))))))))
