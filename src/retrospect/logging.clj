(ns retrospect.logging
  (:use [clojure.string :only [join]])
  (:use [retrospect.state :only [batch logging-enabled]]))

(def ^:dynamic reason-log (ref '()))

(defn log
  [& objs]
  (do
    (when (and (not @batch))
      (dosync (alter reason-log conj (join " " (map str objs)))))
    (when @logging-enabled
      (apply println objs))))
