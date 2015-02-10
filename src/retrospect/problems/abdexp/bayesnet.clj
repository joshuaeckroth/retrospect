(ns retrospect.problems.abdexp.bayesnet
  (:require [clojure.string :as str])
  #_(:import (norsys.netica Environ Net NetTester Node NodeList Streamer))
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [retrospect.state :only [batch]]))

;; (Environ. "+EckrothJ/OhioStateU/Ex14-06-30,121,310/48453")
(def netica-env nil)

;; Netica crashes if nets are garbage collected; save them all
(def bns (ref []))

(defn load-dne
  [file]
  (comment (Net. (Streamer. file))))

(defn save-dne
  [bn file]
  (comment (.write bn (Streamer. file))))

(defn build-bayesnet
  [expgraph]
  (comment
    (let [bn (Net.)
          nodes-map (reduce (fn [m v]
                              (let [states (str/join "," (sort (values expgraph v)))]
                                (assoc m v (Node. v states bn))))
                            {} (vertices expgraph))
          vertex-positions (if @batch {} (gen-vertex-graph-positions expgraph))]
      (doseq [v (keys nodes-map)
              p (sort (explainers expgraph v))]
        (.addLink (nodes-map v) (nodes-map p)))
      (doseq [v (keys nodes-map)]
        (if (empty? (explainers expgraph v))
          (.setCPTable (nodes-map v) (int-array [])
                       (float-array (vals (get (:map (probs expgraph v)) #{}))))
          (doseq [p-states (keys (:map (probs expgraph v)))]
            (.setCPTable (nodes-map v)
                         (str/join "," (map second (sort-by first p-states)))
                         (float-array (vals (get (:map (probs expgraph v)) p-states)))))))
      ;; create a NAND node for each conflict pair
      (doseq [[[v1 val1] [v2 val2]] (conflicts expgraph)]
        (let [nand (Node. (format "%s_%s_C_%s_%s" v1 val1 v2 val2) "on,off" bn)]
          (.setKind nand Node/CONSTANT_NODE)
          (.enterState (.finding nand) "on")
          (.addLink nand (nodes-map v1))
          (.addLink nand (nodes-map v2))
          (doseq [val1-tmp (sort (values expgraph v1))
                  val2-tmp (sort (values expgraph v2))]
            (if (and (= val1-tmp val1) (= val2-tmp val2))
              ;; NAND node "on" probability is 0.0 only if both parents
              ;; take their conflicting state
              (.setCPTable nand (format "%s,%s" val1-tmp val2-tmp) (float-array [0.0 1.0]))
              ;; otherwise NAND node "on" probability is 1.0
              (.setCPTable nand (format "%s,%s" val1-tmp val2-tmp) (float-array [1.0 0.0]))))))
      (doseq [v (keys vertex-positions)]
        (let [[x y] (vertex-positions v)]
          (.setPosition (.visual (.getNode bn v)) x y)))
      ;; Netica crashes if nets are garbage collected; save them all
      (dosync (alter bns conj bn))
      (.compile bn)
      bn)))

(defn copy-net
  [bn]
  (comment (Net. bn "copy" netica-env "")))

(defn get-posterior
  ([bn pairs]
    (comment
      (do
        (.compile bn)
        (let [nodes-vals (map (fn [[v val]] [(.getNode bn v) val]) pairs)
              nodelist (NodeList. bn)]
          (doseq [[node _] nodes-vals]
            (.add nodelist node))
          (let [nodeidxs (map (fn [[node val]] (.getIndex (.state node val)))
                              nodes-vals)]
            (.getJointProbability bn nodelist (int-array nodeidxs))))))
    0.0)
  ([bn vertex value]
     (get-posterior bn [[vertex value]])))

(defn observe
  [bn vertex value]
  ;; may throw an exception if the observation was already made
  ;; with a different state; this can occur if a false vertex
  ;; state was accepted, and later a different state for same
  ;; vertex was observed; recall that observations are not
  ;; rejected when they conflict with accepted hyps (in
  ;; reason.abduction.workspace)
  (comment
    (try (.enterState (.finding (.getNode bn vertex)) value)
         (catch Exception _))))

(defn observe-seq
  [bn obs-seq]
  (doseq [[v val] (filter not-empty obs-seq)]
    (observe bn v val)))

(defn unobserve
  [bn vertex]
  (comment (.clear (.finding (.getNode bn vertex)))))

(defn unobserve-all
  [bn]
  (comment (.retractFindings bn)))

(defn write-findings
  [bn]
  (comment (.writeFindings bn (Streamer. "findings.cas") nil -1 -1.0)))

(defn absorb-vertices
  [bn vertices]
  (comment
    (.compile bn)
    (let [nodelist (NodeList. bn)]
      (doseq [vertex vertices]
        (.add nodelist (.getNode bn vertex)))
      (.absorbNodes bn nodelist))))

(defn conditional-delta
  [bn observed pairs conditioning-pairs]
  (unobserve-all bn)
  (observe-seq bn (filter #(not ((set (concat pairs conditioning-pairs)) %))
                     observed))
  (let [prior (get-posterior bn pairs)]
    (observe-seq bn conditioning-pairs)
    (- (get-posterior bn pairs) prior)))

(defn most-probable-explanation
  [bn]
  (comment
    (.compile bn)
    (let [nodelist (.getNodes bn)
          ;; MPE in Netica does not support querying a subset
          nodeidxs (.getMostProbableConfig bn nodelist)]
      {:states
             (into {} (filter (fn [[node-name _]] (not (re-find #"_C_" node-name)))
                              (map (fn [node idx] [(.getName node) (.getName (.state node idx))])
                                   nodelist nodeidxs)))
       :prob (.getJointProbability bn nodelist nodeidxs)}))
  {:stats [] :prob 0.0})

