(ns samre.problems.tracking.entities)

(defprotocol EntityMethods
  (pos [this]))

(defprotocol SnapshotMethods
  (add-snapshot [this snapshot]))

(defrecord EntitySnapshot [time pos]
  Object
  (toString [_] (format "%s@%d" (str pos) time)))

(defrecord Entity [id snapshots]
  SnapshotMethods
  (add-snapshot
   [this snapshot]
   (update-in this [:snapshots] conj snapshot))
  EntityMethods
  (pos [_] (:pos (last snapshots)))
  Object
  (toString [_] (format "Entity %s %s" id
                        (if (>= 3 (count snapshots))
                          (apply str (interpose "->" (map str snapshots)))
                          (format "%s->...->%s->%s"
                                  (str (first snapshots))
                                  (str (second (reverse snapshots)))
                                  (str (last snapshots)))))))

(defn new-entity
  [time pos]
  (Entity. (hash [time pos]) [(EntitySnapshot. time pos)]))

(defn print-entities
  [entities]
  (map #(println (str %)) entities))

(defn pair-snapshots
  [entity]
  (for [i (range 1 (count (:snapshots entity)))]
    [(nth (:snapshots entity) (dec i)) (nth (:snapshots entity) i)]))



