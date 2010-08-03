(ns simulator.types.events
  (:use [simulator.types.generic :only (Printable)]))

(defrecord EventNew [time pos]
  Printable
  (toStr [this] (format "EventNew: (%d,%d)@%d\n" (:x (:pos this)) (:y (:pos this)) (:time this))))

(defrecord EventMove [time oldpos newpos]
  Printable
  (toStr [this] (format "EventMove: (%d,%d)->(%d,%d)@%d\n"
			(:x (:oldpos this)) (:y (:oldpos this))
			(:x (:newpos this)) (:y (:newpos this))
			(:time this))))