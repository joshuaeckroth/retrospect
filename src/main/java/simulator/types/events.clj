(ns simulator.types.events
  (:use [simulator.types.generic :only (Printable)]))

(defrecord EventNew [time pos]
  Printable
  (to-str [this] (format "At %d, EventNew: (%d,%d)\n" time (:x pos) (:y pos))))

(defrecord EventMove [time oldpos newpos]
  Printable
  (to-str [this] (format "At %d, EventMove: (%d,%d)->(%d,%d)\n"
			 time (:x oldpos) (:y oldpos) (:x newpos) (:y newpos))))