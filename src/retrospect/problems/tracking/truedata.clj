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
  [movements time mean max-walk]
  (reduce #(walk %1 %2 time mean max-walk) movements (my-shuffle (entities movements))))

(defn generate-movements
  [mean steps]
  (let [movements (add-new-entities
                   (new-movements (:GridWidth params) (:GridHeight params))
                   (:NumberEntities params))]
    (loop [time 1
           m movements]
      (if (> time steps) m
          (recur (inc time) (random-walks m time mean (:MaxWalk params)))))))

(defn random-movements
  [steps]
  (let [movements (add-new-entities
                   (new-movements (:GridWidth params) (:GridHeight params))
                   (:NumberEntities params))
        grid-length-avg (/ (double (+ (:GridWidth params) (:GridHeight params))) 2.0)]
    (loop [time 1
           m movements]
      (if (> time steps) m
          (recur (inc time)
                 (random-walks m time (* grid-length-avg (my-rand)) (:MaxWalk params)))))))

(defn generate-truedata
  []
  (let [test-movements (generate-movements (:TrueMoveMean params)
                                           (:Steps params))
        testing-movs (apply concat (vals test-movements))
        training-movs (filter :ot (concat (apply concat (vals (random-movements
                                                          (:TrainingRandom params))))
                                     (apply concat (vals (generate-movements
                                                          (:BelMoveMean params)
                                                          (:TrainingSteps params))))))]
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
