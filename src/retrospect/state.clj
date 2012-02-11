(ns retrospect.state)

(def reason (ref nil))
(def problem (ref nil))
(def datadir (ref nil))
(def database (ref nil))
(def db-params (ref nil))

(def last-id 0)

(defn set-last-id
  [n]
  (if (or (= "AWT-EventQueue-0" (. (Thread/currentThread) getName))
          (= "Thread-1" (. (Thread/currentThread) getName))) 
    (def last-id n)
    (var-set (var last-id) n)))

;; need params not to be shared among threads
(def params nil)

;; used by player only
(def or-state (ref nil))
(def sensors (ref nil))
(def truedata (ref nil))
(def time-now (ref -1))
(def time-prev (ref -1))
