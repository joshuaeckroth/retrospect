(ns retrospect.gui.logs
  (:import (java.awt GridBagLayout Insets))
  (:use [clj-swing.label])
  (:use [clj-swing.text-field])
  (:use [clj-swing.tree])
  (:use [clj-swing.panel])
  (:use [retrospect.workspaces :only [get-hyps]])
  (:use [retrospect.state]))

(def truedata-log (ref ""))
(def problem-log (ref ""))
(def problem-log-label (label ""))
(def hyp-choices (ref []))
(def hyp-selected (atom nil))
(def hyp-info (ref ""))

(def abduction-tree-map (ref {{:id "Root"} {{:id "blah"}}}))

(defn update-logs
  []
  (dosync
   (alter truedata-log (constantly ((:get-truedata-log (:player-fns @problem)))))
   (alter problem-log (constantly ((:get-problem-log (:player-fns @problem)))))
   (alter hyp-choices
          (constantly (sort (map :id (get-hyps (:workspace (:ep-state @or-state))))))))
  (. problem-log-label setText (format "Problem log for: %s" (str (:ep-state @or-state)))))

(comment
  (defn update-hyp-box
    []
    (if *ep-state*
      (if-let [hyp (if-let [choice (.getSelectedItem *hyp-choice*)]
                     (find-first #(= (:id %) choice) (get-hyps (:workspace *ep-state*))))]
        (. *hyp-box* setText
           (apply str (:desc hyp) "\n\n"
                  (interpose
                   "\n" (map str (filter (fn [l] (some #(= % (:id hyp)) (:hyp-ids l)))
                                         (:abducer-log (:workspace *ep-state*))))))))
      (. *hyp-box* setText ""))))

(defn logs-tab
  []
  (panel :layout (GridBagLayout.)
         :constrains (java.awt.GridBagConstraints.)
         [:gridx 0 :gridy 0 :weightx 1.0 :weighty 1.0 :fill :BOTH :insets (Insets. 5 5 5 5)
          _ (text-field :str-ref truedata-log)

          :gridx 0 :gridy 1 :weighty 0.0
          _ problem-log-label
          :gridx 0 :gridy 2 :weighty 1.0
          _ (text-field :str-ref problem-log)

          :gridx 0 :gridy 3
          _ (tree :model (mapref-tree-model abduction-tree-map {:id "Root"}))]))