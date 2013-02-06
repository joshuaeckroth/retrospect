(ns retrospect.reason.abduction.gui.problem-log
  (:import (java.awt GridBagLayout Insets))
  (:use [clj-swing.label])
  (:use [clj-swing.panel])
  (:use [retrospect.epistemicstates :only [cur-ep]])
  (:use [retrospect.gui.common])
  (:use [retrospect.state]))

(def truedata-log (ref ""))
(def problem-log (ref ""))
(def problem-log-label (label ""))

(defn update-problem-log
  []
  (dosync
   (alter truedata-log (constantly ((:get-truedata-log (:player-fns @problem)))))
   (alter problem-log (constantly ((:get-problem-log (:player-fns @problem))))))
  (. problem-log-label setText (format "Problem log for: %s" (str (cur-ep (:est @or-state))))))

(defn problem-log-tab
  []
  (doto
      (split-vertical
       (log-box truedata-log)
       (panel :layout (GridBagLayout.)
              :constrains (java.awt.GridBagConstraints.)
              [:gridx 0 :gridy 0 :weightx 1.0 :weighty 0.0
               :fill :BOTH :insets (Insets. 5 0 5 0)
               _ problem-log-label
               :gridy 1 :weighty 1.0
               _ (log-box problem-log)]))
    (.setDividerLocation 300)))