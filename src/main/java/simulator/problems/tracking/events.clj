(ns simulator.problems.tracking.events)

(defrecord EventNew [time pos]
  Object
  (toString [_] (format "EventNew: (%d,%d)@%d" (:x pos) (:y pos) time)))

(defrecord EventMove [time oldpos pos]
  Object
  (toString [_] (format "EventMove: (%d,%d)->(%d,%d)@%d"
			(:x oldpos) (:y oldpos) (:x pos) (:y pos) time)))

(defrecord EventFrozen [time pos]
  Object
  (toString [_] (format "EventFrozen: (%d,%d)@%d" (:x pos) (:y pos) time)))
