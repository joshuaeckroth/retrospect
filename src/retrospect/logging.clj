(ns retrospect.logging
  (:use [clojure.string :only [join]])
  (:use [retrospect.state :only [batch]])
  (:use [retrospect.profile :only [prof]]))

(def reason-log (ref '()))

(defn log
  [& objs]
  (prof :log
        (do
          #_(apply println objs)
          (when (not @batch)
            (dosync (alter reason-log conj (join " " (map str objs))))))))