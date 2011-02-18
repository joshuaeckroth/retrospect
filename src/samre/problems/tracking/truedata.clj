(ns samre.problems.tracking.truedata
  (:use [samre.problems.tracking.grid])
  (:use [samre.problems.tracking.prepared]))

(defn add-new-entities
  [grid numes]
  (reduce (fn [g _] (grid-new-entity g 0)) grid (range numes)))

(defn random-walks
  [grid params]
  (let [maxwalk (:MaxWalk params)
        es (filter (fn [_] (<= (rand) (double (/ (:ProbMovement params) 100))))
                   (grid-entities grid))
        es-walk (shuffle (flatten (map (fn [e] (repeat (inc (rand-int maxwalk)) e)) es)))]
    (reduce (fn [g e] (walk1 g e)) grid es-walk)))

(defn possibly-add-new-entity
  [grid time params]
  (if (>= (double (/ (:ProbNewEntities params) 100)) (rand))
    (grid-new-entity grid time)
    grid))

(defn generate-truedata
  [params]
  (let [grid (add-new-entities (new-grid (:GridWidth params) (:GridHeight params))
                               (:NumberEntities params))]
    (loop [time 1
           truedata [grid]]
      (if (> time (:Steps params)) truedata
          (let [newgrid (-> (last truedata)
                            (random-walks params)
                            (possibly-add-new-entity time params)
                            (update-all-entity-times time))]
            (recur (inc time) (conj truedata newgrid)))))))