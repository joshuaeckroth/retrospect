(ns retrospect.logging
  (:use [retrospect.state]))

(defn log
  [& objs]
  #_(apply println objs)
  (when (not @batch)
    (dosync (alter reason-log str "\n" (apply str (interpose " " (map str objs)))))))