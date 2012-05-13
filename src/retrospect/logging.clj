(ns retrospect.logging
  (:use [retrospect.state :only [batch]])
  (:use [retrospect.profile :only [prof]]))

(def reason-log "")

(defn log
  [& objs]
  (prof :log
        #_(apply println objs)
        (when (not @batch)
          (def reason-log (str reason-log "\n" (apply str (interpose " " (map str objs))))))))