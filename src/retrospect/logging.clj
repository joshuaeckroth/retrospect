(ns retrospect.logging
  (:use [clojure.string :only [join]])
  (:use [retrospect.state :only [batch logging-enabled]])
  (:use [retrospect.profile :only [*enable-profiling* prof]]))

(def ^:dynamic reason-log (ref '()))

(defn log
  [& objs]
  (prof :log
        (do
          (when (and (not @batch) (not *enable-profiling*))
            (dosync (alter reason-log conj (join " " (map str objs)))))
          (when @logging-enabled
            (apply println objs)))))
