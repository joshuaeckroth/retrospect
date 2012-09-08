(ns retrospect.logging
  (:use [clojure.string :only [join]])
  (:use [retrospect.state :only [batch logging-enabled]])
  (:use [retrospect.profile :only [prof]]))

(def reason-log (ref '()))

(defn log
  [& objs]
  (prof :log
        (do
          (when (not @batch)
            (dosync (alter reason-log conj (join " " (map str objs)))))
          (when logging-enabled
            (apply println objs)))))