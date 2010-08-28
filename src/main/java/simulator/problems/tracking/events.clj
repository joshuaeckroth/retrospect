(ns simulator.problems.tracking.events)

(defrecord EventNew [time pos]
  Object
  (toString [_] (format "At %d, EventNew: (%d,%d)\n" time (:x pos) (:y pos))))

(defrecord EventMove [time oldpos newpos]
  Object
  (toString [_] (format "At %d, EventMove: (%d,%d)->(%d,%d)\n"
			time (:x oldpos) (:y oldpos) (:x newpos) (:y newpos))))