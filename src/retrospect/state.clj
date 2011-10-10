(ns retrospect.state)

(def problem (ref nil))
(def datadir (ref nil))
(def database (ref nil))
(def db-params (ref nil))

;; need params not to be shared among threads
(def params nil)

;; used by player only
(def or-state (ref nil))
(def sensors (ref nil))
(def truedata (ref nil))
(def time-now (ref -1))
(def time-prev (ref -1))
