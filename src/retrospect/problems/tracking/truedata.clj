(ns retrospect.problems.tracking.truedata
  (:use [retrospect.random])
  (:use [retrospect.colors])
  (:use [retrospect.problems.tracking.grid])
  (:use [retrospect.problems.tracking.prepared])
  (:use [clojure.contrib.seq :only [find-first]]))

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

(defn get-grid-movements
  [truedata mintime maxtime]
  (if (< maxtime 0) []
      (flatten
       (for [time (range mintime (inc maxtime))]
         (let [grid-before (nth truedata time)
               grid-after (nth truedata (inc time))]
           (map (fn [e] (let [e2 (find-first #(= e %) (grid-entities grid-after))]
                          {:e e :ox (:x (meta e)) :oy (:y (meta e)) :ot (:time (meta e))
                           :x (:x (meta e2)) :y (:y (meta e2)) :t (:time (meta e2))}))
                (grid-entities grid-before)))))))

(defn export-truedata
  [truedata]
  (let [moves (get-grid-movements truedata 0 (dec (dec (count truedata))))
        entities (set (map :e moves))
        spaces (fn [ss] (apply str (interpose " " ss)))
        entity-pos-list
        (fn [e] (spaces (concat (let [m (find-first #(= e (:e %)) moves)]
                                  [(format "%d,%d" (:ox m) (:oy m))])
                                (for [m moves :when (= (:e m) e)]
                                  (format "%d,%d" (:x m) (:y m))))))]
    (format "(build-truedata params (entity-paths %s))"
            (apply str (map (fn [e] (format "[\"%s\" %s %d %s]\n"
                                            (str e) (color-str (:color (meta e)))
                                            (:ot (find-first #(= e (:e %)) moves))
                                            (entity-pos-list e)))
                            entities)))))
