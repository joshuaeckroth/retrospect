(ns samre.problems.tracking.events)

(defrecord EventNew [time pos]
  Object
  (toString [_] (format "EventNew: (%d,%d)@%d" (:x pos) (:y pos) time)))

(defrecord EventAppear [time pos]
  Object
  (toString [_] (format "EventAppear: (%d,%d)@%d" (:x pos) (:y pos) time)))

(defrecord EventMove [time oldtime pos oldpos]
  Object
  (toString [_] (format "EventMove: (%d,%d)@%d->(%d,%d)@%d"
			(:x oldpos) (:y oldpos) oldtime (:x pos) (:y pos) time)))

(defrecord EventFrozen [time oldtime pos oldpos]
  Object
  (toString [_] (format "EventFrozen: (%d,%d)@%d-%d" (:x pos) (:y pos) oldtime time)))

(defrecord EventDisappear [time oldtime pos oldpos]
  Object
  (toString [_] (format "EventDisappear: (%d,%d)@%d->(?,?)->(%d,%d)@%d"
                        (:x oldpos) (:y oldpos) oldtime
                        (:x pos) (:y pos) time)))

