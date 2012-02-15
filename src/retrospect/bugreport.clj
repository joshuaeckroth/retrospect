(ns retrospect.bugreport
  (:import (java.awt GridBagLayout Insets))
  (:import (java.io StringWriter PrintWriter))
  (:import (org.bug4j.client Bug4jAgent))
  (:use [clj-swing.panel])
  (:use [clj-swing.button])
  (:use [clj-swing.frame])
  (:use [clj-swing.text-field]))

(def exception (ref ""))

(defn set-exception-handler
  []
  (Thread/setDefaultUncaughtExceptionHandler
   (proxy [Thread$UncaughtExceptionHandler] []
     (uncaughtException [thread throwable]
       (dosync (alter exception (constantly
                                 (let [s (StringWriter.)]
                                   (.printStackTrace throwable (PrintWriter. s))
                                   (str s)))))
       (frame :title "Exception"
              :layout (GridBagLayout.)
              :constrains (java.awt.GridBagConstraints.)
              :size [700 500]
              :show true
              :on-close :hide
              [:gridx 0 :gridy 0 :weightx 1.0 :weighty 1.0 :gridwidth 2
               :fill :BOTH :insets (Insets. 5 5 5 5)
               _ (scroll-panel (text-area :str-ref exception :editable false
                                          :wrap false :rows 30))
               :gridx 0 :gridy 1 :weighty 0.0 :gridwidth 1
               _ (button "Report crash"
                         :action ([_] (Bug4jAgent/report "Uncaught exception"
                                                         throwable)))
               :gridx 1
               _ (button "Close app" :action ([_] (Bug4jAgent/shutdown)
                                                (System/exit -1)))])
       (Bug4jAgent/shutdown)))))
