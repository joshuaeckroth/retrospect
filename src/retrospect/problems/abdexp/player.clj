(ns retrospect.problems.abdexp.player
  (:import (java.awt GridBagLayout Insets))
  (:import (javax.swing JScrollPane JTabbedPane))
  (:import (norsys.netica.gui NetPanel NodePanel))
  (:require [clojure.string :as str])
  (:use [loom.graph :only [digraph]])
  (:use [clj-swing.core :only [add-action-listener]])
  (:use [clj-swing.button])
  (:use [clj-swing.panel])
  (:use [clj-swing.label])
  (:use [clj-swing.text-field])
  (:use [retrospect.gui.graphs])
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [retrospect.problems.abdexp.bayesnet])
  (:use [retrospect.epistemicstates :only [cur-ep]])
  (:use [retrospect.reason.abduction.workspace :only [lookup-hyp]])
  (:use [retrospect.state]))

(def canvas (ref nil))
(def net-panel (ref nil))
(def net-panel-scrollpane (ref nil))

(def table-contents (ref "Click a node."))

(def current-expgraph-dot (ref ""))
(def current-expgraph-svg (ref ""))

(def prec-label (label ""))
(def unexp-label (label ""))
(def noexp-label (label ""))

(defn gen-values
  [vs]
  (let [expgraph (:expgraph @truedata)
        vertex (first vs)
        vals (values expgraph vertex)]
    (if (empty? (rest vs)) (map (fn [v] [v]) vals)
        (let [other-vals (gen-values (rest vs))]
          (mapcat (fn [v] (map (fn [ov] (concat [v] ov)) other-vals)) vals)))))

(defn get-probs-table
  [vertex]
  (when (vertex? (:expgraph @truedata) vertex)
    (let [expgraph (:expgraph @truedata)
          vals (values expgraph vertex)
          ps (:table (probs expgraph vertex))
          expl (sort (explainers expgraph vertex))
          table (gen-values (concat [vertex] expl))]
      {:vertices (concat [vertex] expl) :table table :probs ps})))

(defn listener
  [vertex]
  (let [bn (:bayesnet @truedata)
        expgraph (:expgraph @truedata)]
    (dosync (alter table-contents
                   (constantly
                    (format "%s\n\nPriors: %s\n\nObserved: %s\n\nPosteriors: %s\n\nProbs:\n\n%s"
                       vertex
                       ;; priors
                       (do (unobserve-all bn)
                           (str/join ", " (for [v (sort (values expgraph vertex))]
                                            (format "%s=%.2f" v
                                               (get-posterior bn vertex v)))))
                       ;; observed
                       (if-let [val (second (first (filter (fn [[n v]] (= n vertex))
                                                      (apply concat (take (inc @time-now)
                                                                          (:test @truedata))))))]
                         val "(not observed)")
                       ;; posteriors
                       (do (unobserve-all bn)
                           (observe-seq bn (apply concat (take (inc @time-now)
                                                               (:test @truedata))))
                           (str/join ", " (for [v (sort (values expgraph vertex))]
                                            (format "%s=%.2f" v
                                               (get-posterior bn vertex v)))))
                       ;; probs table
                       (if (empty? (:table (probs expgraph vertex))) ""
                           (let [{:keys [vertices table probs]} (get-probs-table vertex)]
                             (when vertices (format "%s\n\n%s" (str/join ", " vertices)
                                               (str/join "\n"
                                                         (for [i (range (count probs))]
                                                           (format "%s %.2f" (str/join ", " (nth table i))
                                                              (nth probs i))))))))))))))

(defn player-get-stats-panel
  []
  (panel :layout (GridBagLayout.)
         :constrains (java.awt.GridBagConstraints.)
         [:gridx 0 :gridy 0 :weightx 1.0 :weighty 0.0
          :fill :BOTH :insets (Insets. 5 5 5 5)
          :gridx 0
          _ (label "Prec:")
          :gridx 1
          _ prec-label
          :gridx 0 :gridy 1
          _ (label "Unexplained:")
          :gridx 1
          _ unexp-label
          :gridx 0 :gridy 2
          _ (label "NoExplainers:")
          :gridx 1
          _ noexp-label]))

(defn player-update-stats
  []
  (if-let [results (last (:results (cur-ep (:est @or-state))))]
    (do
      (. prec-label (setText (format "%.2f" (:Prec results))))
      (. unexp-label (setText (format "%.2f" (:UnexplainedPct results))))
      (. noexp-label (setText (format "%.2f" (:NoExplainersPct results)))))
    (do
      (. prec-label (setText "N/A"))
      (. unexp-label (setText "N/A"))
      (. noexp-label (setText "N/A")))))

(defn generate-expgraph
  []
  (when (:expgraph @truedata)
    (generate-graph (format-dot-expgraph (:expgraph @truedata)
                                         (:true-values-map @truedata))
                    @canvas listener false
                    current-expgraph-dot current-expgraph-svg)))

(defn player-update-diagram
  []
  (when (:bayesnet @truedata)
    (if (or (nil? @net-panel) (not= (.getNet @net-panel) (:bayesnet @truedata)))
      (do
        (dosync (alter net-panel
                       (constantly (NetPanel. (:bayesnet @truedata)
                                              NodePanel/NODE_STYLE_AUTO_SELECT))))
        (.setLinkPolicy @net-panel NetPanel/LINK_POLICY_BELOW)
        (.setView (.getViewport @net-panel-scrollpane) @net-panel))
      (do
        (unobserve-all (:bayesnet @truedata))
        (observe-seq (:bayesnet @truedata)
                     (apply concat (take (:time (cur-ep (:est @or-state)))
                                         (:test @truedata))))
        (.compile (:bayesnet @truedata))
        (.refreshDataDisplayed @net-panel)))))

(defn player-setup-diagram
  []
  (dosync (alter canvas (constantly (create-canvas))))
  (dosync (alter net-panel-scrollpane (constantly (JScrollPane.))))
  (panel :layout (GridBagLayout.)
         :constrains (java.awt.GridBagConstraints.)
         [:gridx 0 :gridy 0 :weightx 1.0 :weighty 1.0
          :fill :BOTH :insets (Insets. 5 5 5 5)
          _ (let [tabs (JTabbedPane.)]
              (doto tabs
                (.addTab "Expgraph"
                         (panel :layout (GridBagLayout.)
                                :constrains (java.awt.GridBagConstraints.)
                                [:gridx 0 :gridy 0 :weightx 1.0 :weighty 1.0
                                 :fill :BOTH :insets (Insets. 5 5 5 5)
                                 _ @canvas
                                 :gridy 1 :weighty 0.3
                                 _ (scroll-panel (text-area :str-ref table-contents
                                                            :editable false :wrap false))
                                 :gridy 2 :gridx 0 :weighty 0.0
                                 _ (panel :layout (GridBagLayout.)
                                          :constrains (java.awt.GridBagConstraints.)
                                          [:gridx 0 :gridy 0 :weightx 1.0 :weighty 1.0
                                           _ (panel)
                                           :gridx 1 :weightx 0.0
                                           _ (doto (button "Save Dot")
                                               (add-action-listener ([_] (save-dot @current-expgraph-dot))))
                                           :gridx 2
                                           _ (doto (button "Save SVG")
                                               (add-action-listener ([_] (save-svg @current-expgraph-svg))))
                                           :gridx 3
                                           _ (doto (button "Generate")
                                               (add-action-listener ([_] (generate-expgraph))))])]))
                (.addTab "Bayes net" @net-panel-scrollpane)))]))

(defn player-get-truedata-log
  []
  (if (<= @time-now 0) ""
      (format "True values:\n%s\n\nFalse values:\n%s"
         (str/join ", " (sort (map (fn [[vertex value]] (format "%s=%s" vertex value))
                                 (:true-values-map @truedata))))
         (str/join ", " (sort (map (fn [[vertex value]] (format "%s=%s" vertex value))
                                 (:false-values-map @truedata)))))))

(defn player-get-problem-log
  []
  (if (<= @time-now 0) ""
      (let [ws (:workspace (cur-ep (:est @or-state)))]
        (str "Believed explainers: "
             (str/join ", " (sort (set (map (fn [h] (format "%s=%s" (:vertex h) (:value h)))
                                          (map #(lookup-hyp ws %)
                                             (get (:accepted ws) :expl))))))))))
