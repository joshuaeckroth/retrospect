(ns simulator.problems.tracking.truedata
  (:use [simulator.problems.tracking.grid :only
         (new-grid new-entity replace-grid-entity walk1)])
  (:use [simulator.problems.tracking.eventlog :only
         (init-event-log add-entity add-event
                         add-event-new add-event-move update-entity
                         get-entities get-events)])
  (:use [simulator.problems.tracking.entities :only (pos pair-snapshots)])
  (:use [clojure.set :only [difference]]))

(defn add-new-entities
  [eventlog grid numes time]
  (loop [i 0
         el eventlog
	 g grid]
    (if (or (= i numes) ; stop if reached numes or if there's no more space in grid
	    (= (* (:width g) (:height g))
	       (count (get-entities el))))
      [el g]
      (let [entity (new-entity g time)]
	(recur (inc i)
	       (-> el
		   (add-entity entity)
		   (add-event-new time (pos entity)))
	       (replace-grid-entity g nil entity))))))

(defn random-walks
  [eventlog grid params time]
  (let [all-entities
        (get-entities eventlog)
        
        moving-entities
        (filter (fn [_] (<= (rand) (double (/ (:ProbMovement params) 100))))
                (shuffle all-entities))
        
        non-moving-entities
        (difference (set all-entities) (set moving-entities))
        
        frozen-eventlog
        (reduce (fn [te olde] (update-entity te time olde (pos olde)))
                eventlog non-moving-entities)
        
        entities-map
        (if (empty? moving-entities) {}
            (apply assoc {} (interleave moving-entities moving-entities)))
        
        entity-walks
        (if (empty? moving-entities) []
            (shuffle (apply concat (map #(repeat (inc (rand-int (:MaxWalk params))) %)
                                        moving-entities))))]
    (loop [em entities-map
           g grid
           ew entity-walks]
      (if (empty? ew)
        [(reduce (fn [fe olde]
                   (if (= (pos olde) (pos (get em olde)))
                     ;; entity didn't move afterall
                     (-> fe (update-entity time olde (pos olde)))
                     ;; entity moved
                     (-> fe
                         (update-entity time olde (pos (get em olde)))
                         (add-event-move time (pos olde) (pos (get em olde))))))
                 frozen-eventlog (keys em))
         g]
        (let [e (first ew)
              olde (get em e)
              newe (walk1 olde g time)]
          (if newe
            (recur (assoc em e newe)
                   (replace-grid-entity g olde newe)
                   (rest ew))
            (recur em g (rest ew))))))))

(defn possibly-add-new-entities
  [eventlog grid params time]
  (if (>= (double (/ (:ProbNewEntities params) 100)) (rand))
    (add-new-entities eventlog grid 1 time)
    [eventlog grid]))

(defn generate-truedata
  [params]
  (let [[el grid]
        (add-new-entities (init-event-log) (new-grid params)
                          (:NumberEntities params) 0)]
    (loop [time 0
           truedata [{:eventlog el :grid grid}]]
      (if (> time (:Steps params)) truedata
          (let [[el2 g2] (random-walks (:eventlog (last truedata))
                                       (:grid (last truedata))
                                       params (inc time))
                [el3 g3] (possibly-add-new-entities el2 g2 params (inc time))]
            (recur (inc time) (conj truedata {:eventlog el3 :grid g3})))))))