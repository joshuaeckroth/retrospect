(ns samre.problems.tracking.entities
  (:use [samre.colors]))

(defprotocol EntityMethods
  (pos [this]))

(defprotocol SnapshotMethods
  (add-snapshot [this snapshot]))

(defrecord EntitySnapshot [time pos]
  Object
  (toString [_] (format "%s@%d" (str pos) time)))

(defrecord Entity [id color snapshots]
  SnapshotMethods
  (add-snapshot
   [this snapshot]
   (update-in this [:snapshots] conj snapshot))
  EntityMethods
  (pos [_] (:pos (last snapshots)))
  Object
  (toString [_] (format "Entity %s %s %s" id (color-str color)
                        (if (>= 3 (count snapshots))
                          (apply str (interpose "->" (map str snapshots)))
                          (format "%s->...->%s->%s"
                                  (str (first snapshots))
                                  (str (second (reverse snapshots)))
                                  (str (last snapshots)))))))

(defn new-entity
  ([time pos] (new-entity time pos (rand-nth [red blue])))
  ([time pos color]
     (Entity. (hash [(rand) time (:x pos) (:y pos)]) color
              [(EntitySnapshot. time pos)])))

(defn pair-snapshots
  [entity]
  (for [i (range 1 (count (:snapshots entity)))]
    [(nth (:snapshots entity) (dec i)) (nth (:snapshots entity) i)]))


