(ns simulator.problems.tracking.entities)

(defprotocol EntityMethods
  (pos [this]))

(defprotocol SnapshotMethods
  (add-snapshot [this snapshot]))

(defrecord EntitySnapshot [time pos]
  Object
  (toString [_] (str pos)))

(defrecord Entity [snapshots]
  SnapshotMethods
  (add-snapshot
   [this snapshot]
   (update-in this [:snapshots] conj snapshot))
  EntityMethods
  (pos [_] (:pos (last snapshots)))
  Object
  (toString [_] (format "Entity %s"
			(apply str (interpose "->" (map str snapshots))))))

(defn print-entities
  [entities]
  (dorun (map #(println (str %)) entities)))


