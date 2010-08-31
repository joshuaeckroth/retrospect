(ns simulator.problems.tracking.events)

(defrecord EventNew [time pos]
  Object
  (toString [_] (format "EventNew: (%d,%d) @ %d\n" (:x pos) (:y pos) time)))

(defrecord EventMove [time oldpos newpos]
  Object
  (toString [_] (format "EventMove: (%d,%d)->(%d,%d) @ %d\n"
			(:x oldpos) (:y oldpos) (:x newpos) (:y newpos) time)))