(ns retrospect.state)

(def batch (ref false))
(def reasoner (ref nil))
(def problem (ref nil))
(def datadir (ref nil))
(def quiet-mode (ref nil))

(def ^:dynamic last-id 0)

(def ^:dynamic cache)

(def logging-enabled (ref false))

(defn set-last-id
  [n]
  (if (or (= "AWT-EventQueue-0" (. (Thread/currentThread) getName))
          (= "Thread-1" (. (Thread/currentThread) getName)))
    (def last-id n)
    (var-set (var last-id) n)))

;; params, training? need not to be shared among threads
(def ^:dynamic params nil)
(def ^:dynamic training? false)

;; generally only used by the player/explore mode
(def results (ref []))
(def or-state (ref nil))
(def sensors (ref nil))
(def truedata (ref nil))
(def time-now (ref -1))
(def time-prev (ref -1))
