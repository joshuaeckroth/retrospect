(ns retrospect.logging
  (:use [clojure.string :only [join]])
  (:use [retrospect.state :only [batch]])
  (:use [retrospect.profile :only [prof]]))

(def reason-log '())

(defn log
  [& objs]
  (prof :log
        (do
          #_(apply println objs)
          (when (not @batch)
            (def reason-log (conj reason-log (join " " (map str objs))))))))