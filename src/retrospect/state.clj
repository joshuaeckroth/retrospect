(ns retrospect.state)

(def batch (ref false))
(def reasoner (ref nil))
(def problem (ref nil))
(def datadir (ref nil))
(def db-params (ref nil))

(def last-id 0)

(def logging-enabled (ref false))

(defn set-last-id
  [n]
  (if (or (= "AWT-EventQueue-0" (. (Thread/currentThread) getName))
          (= "Thread-1" (. (Thread/currentThread) getName))
          (= "Swank REPL Thread" (. (Thread/currentThread) getName)))
    (def last-id n)
    (var-set (var last-id) n)))

;; params, training? need not to be shared among threads
(def params nil)
(def training? false)

;; generally only used by the player/explore mode
(def results (ref []))
(def or-state (ref nil))
(def sensors (ref nil))
(def truedata (ref nil))
(def time-now (ref -1))
(def time-prev (ref -1))
