(ns retrospect.logging
  (:use [retrospect.state :only [batch]]))

(def reason-log "")

(defn log
  [& objs]
  #_(apply println objs)
  #_(when (not @batch)
      (def reason-log (str reason-log "\n" (apply str (interpose " " (map str objs)))))))