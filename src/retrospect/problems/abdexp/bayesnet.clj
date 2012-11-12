(ns retrospect.problems.abdexp.bayesnet
  (:require [clojure.string :as str])
  (:import (norsys.netica Environ Net NetTester Node NodeList Streamer))
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [retrospect.profile :only [prof]]))

(def netica-env (Environ. nil))

(defn load-dne
  [file]
  (Net. (Streamer. file)))

(defn save-dne
  [bn file]
  (.write bn (Streamer. file)))

;; TODO: support conflicts
(defn build-bayesnet
  [expgraph]
  (let [bn (Net.)
        nodes-map (reduce (fn [m v]
                       (let [states (str/join "," (sort (values expgraph v)))]
                         (assoc m v (Node. v states bn))))
                     {} (vertices expgraph))]
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
    bn))

(defn get-posterior
  ([bn pairs]
     (prof :bn-get-posterior
           (do
             (.compile bn)
             (let [nodes-vals (map (fn [[v val]] [(.getNode bn v) val]) pairs)
                   nodelist (NodeList. bn)]
               (doseq [[node _] nodes-vals]
                 (.add nodelist node))
               (let [nodeidxs (map (fn [[node val]] (.getIndex (.state node val)))
                                 nodes-vals)]
                 (.getJointProbability bn nodelist (int-array nodeidxs)))))))
  ([bn vertex value]
     (get-posterior bn [[vertex value]])))

(defn observe
  [bn vertex value]
  (prof :bn-observe
        (.enterState (.finding (.getNode bn vertex)) value)))

(defn observe-seq
  [bn obs-seq]
  (doseq [[v val] (filter not-empty obs-seq)]
    (observe bn v val)))

(defn unobserve
  [bn vertex]
  (prof :bn-unobserve
        (.clear (.finding (.getNode bn vertex)))))

(defn unobserve-all
  [bn]
  (prof :bn-unobserve-all
        (.retractFindings bn)))

(defn conditional-delta
  [bn pairs conditioning-pairs]
  (unobserve-all bn)
  (let [prior (get-posterior bn pairs)]
    (observe-seq bn conditioning-pairs)
    (- (get-posterior bn pairs) prior)))

(defn most-probable-explanation
  [bn]
  (.compile bn)
  (let [nodelist (.getNodes bn)
        ;; MPE in Netica does not support querying a subset
        nodeidxs (.getMostProbableConfig bn nodelist)]
    {:states
     (reduce (fn [m [node idx]] (assoc m (.getName node) (.getName (.state node idx))))
        {} (partition 2 (interleave nodelist nodeidxs)))
     :prob (.getJointProbability bn nodelist nodeidxs)}))

