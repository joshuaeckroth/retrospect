(ns simulator.types.entities
  (:use [simulator.types.generic :only (Printable toStr)]))

(defprotocol EntityMethods
  (pos [this]))

(defprotocol SnapshotMethods
  (addSnapshot [this snapshot]))

(defrecord EntitySnapshot [pos])

(defrecord Entity [symbol snapshots]
  SnapshotMethods
  (addSnapshot
   [this snapshot]
   (update-in this [:snapshots] conj snapshot))
  EntityMethods
  (pos [this] (:pos (last (:snapshots this))))
  Printable
  (toStr [this] (format "Entity %c %s\n" (:symbol this)
			(apply str (interpose "->" (map #(format "(%d,%d)" (:x (:pos %)) (:y (:pos %)))
							(:snapshots this)))))))

(defn print-entities
  [entities]
  (dorun (map #(println (toStr %)) entities)))


