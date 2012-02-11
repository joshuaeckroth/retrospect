(ns retrospect.bugreport
  (:import (org.bug4j.client Bug4jUncaughtExceptionHandler)))

(defn set-exception-handler
  []
  (Bug4jUncaughtExceptionHandler/install))
