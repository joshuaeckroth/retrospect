(ns simulator.types.entities
  (:use [simulator.types.generic :only (Printable to-str)]))

(defprotocol EntityMethods
  (pos [this]))

(defprotocol SnapshotMethods
  (add-snapshot [this snapshot]))

(defrecord EntitySnapshot [pos]
  Printable
  (to-str [this] (to-str pos)))

(defrecord Entity [symbol snapshots]
  SnapshotMethods
  (add-snapshot
   [this snapshot]
   (update-in this [:snapshots] conj snapshot))
  EntityMethods
  (pos [this] (:pos (last (:snapshots this))))
  Printable
  (to-str [this] (format "Entity %c %s" (:symbol this)
			 (apply str (interpose "->" (map to-str (:snapshots this)))))))

(defn print-entities
  [entities]
  (dorun (map #(println (to-str %)) entities)))


