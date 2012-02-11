(ns retrospect.problems.causal.player
  (:import (java.awt GridBagLayout GridBagConstraints Insets))
  (:use [loom.graph :only [nodes incoming]])
  (:use [loom.attr :only [attr add-attr]])
  (:use [clj-swing.panel])
  (:use [clj-swing.text-field])
  (:use [retrospect.gui.graphs])
  (:use [retrospect.state]))

(def table-contents (ref "Click a node."))

(defn gen-values
  [nodes]
  (let [network (:network @truedata)
        node (first nodes)
        values (attr network node :values)]
    (if (empty? (rest nodes)) (map (fn [v] [v]) values)
        (let [other-values (gen-values (rest nodes))]
          (mapcat (fn [v] (map (fn [ov] (concat [v] ov)) other-values)) values)))))

(defn get-probs-table
  [node]
  (if ((nodes (:network @truedata)) node)
    (let [network (:network @truedata)
          values (attr network node :values)
          probs (attr network node :probs)
          parents (sort (incoming network node))
          table (gen-values (concat [node] parents))]
      {:nodes (concat [node] parents) :table table :probs probs})))

(defn listener
  [node]
  (let [commas (fn [ss] (apply str (interpose ", " ss)))
        lines (fn [ss] (apply str (interpose "\n" ss)))
        {:keys [nodes table probs]} (get-probs-table node)]
    (if nodes
      (dosync (alter table-contents
                     (constantly
                      (format "%s\n\n%s" (commas nodes)
                              (lines (for [i (range (count probs))]
                                       (format "%s:\t%.2f" (commas (nth table i))
                                               (nth probs i)))))))))))

(def canvas (ref nil))

(defn player-setup-diagram
  []
  (dosync (alter canvas (constantly (create-canvas))))
  (let [network (:network @truedata)]
    (generate-graph network @canvas listener false))
  (panel :layout (GridBagLayout.)
         :constrains (GridBagConstraints.)
         [:gridx 0 :gridy 0 :weightx 1.0 :weighty 1.0
          :fill :BOTH :insets (Insets. 5 5 5 5)
          _ @canvas
          :gridx 1 :gridy 0 :weightx 0.3
          _ (scroll-panel (text-area :str-ref table-contents
                                     :editable false :wrap false))]))

(defn player-update-diagram
  []
  (let [network (:network @truedata)]
    (generate-graph network @canvas listener false)))

(defn player-get-stats-panel
  []
  (panel))

(defn player-update-stats
  [])

(defn player-get-truedata-log
  []
  (if (>= @time-now (count (:observed-seq @truedata))) ""
      (str (nth (:observed-seq @truedata) @time-now))))

(defn player-get-problem-log
  []
  (let [pdata (:problem-data (:ep-state @or-state))
        believed (:believed pdata)]
    (apply str (interpose "\n" (map #(format "%s: %s (%s)" %
                                             (get (get believed %) :value)
                                             (:id (get (get believed %) :hyp)))
                                    (sort (keys believed)))))))
