(ns retrospect.workspaces
  (:require [retrospect logs])
  (:import [retrospect.logs AbducerLogEntry])
  (:use [retrospect.confidences])
  (:require [clojure.set :as set])
  (:use [loom.graph :only
         [digraph nodes incoming neighbors weight
          add-nodes add-edges remove-nodes edges]]))

(def last-id (ref 0))

(defn new-hyp
  [prefix type apriori explains desc data]
  (dosync
   (let [id (inc @last-id)]
     (alter last-id inc)
     {:id (format "%s%d" prefix id) :type type
      :explains explains :apriori apriori :desc desc :data data})))

(defn init-workspace
  ([]
     {:graph nil
      :graph-static (digraph)
      :hyp-confidences {}
      :abducer-log []
      :dot []
      :confidence nil
      :accepted []
      :rejected []
      :forced []
      :resources {:explain-cycles 0 :hyp-count 0}})
  ([workspace-old]
     (assoc-in (init-workspace) [:resources :explains-cycles]
               (:explains-cycles (:resources workspace-old)))))

(defn get-hyps
  [workspace]
  (nodes (:graph-static workspace)))

(defn hyp-conf
  [workspace hyp]
  (get (:hyp-confidences workspace) hyp))

(defn sort-by-conf
  [workspace hyps]
  (reverse (sort-by (partial hyp-conf workspace) hyps)))

(defn find-explainers
  ([workspace]
     (let [g (:graph workspace)]
       (set (mapcat #(incoming g %) (nodes g)))))
  ([workspace hyp]
     (let [g (:graph workspace)]
       (incoming g hyp)))
  ([workspace hyp hyps]
     (let [g (:graph workspace)]
       (set/intersection (set hyps) (set (find-explainers workspace hyp))))))

(defn find-unexplained
  "Unexplained hyps are those that are forced and remain in the graph,
   and those that are in the graph and have incoming edges."
  [workspace]
  (let [g (:graph workspace)
        hyps (nodes g)]
    (set (concat (set/intersection (set (:forced workspace)) (set hyps))
                 (filter #(not-empty (incoming g %)) hyps)))))

(defn find-candidates
  [workspace]
  (let [g (:graph workspace)]
    (filter (fn [h] (not-empty (neighbors g h))) (nodes g))))

(defn essential?
  [workspace hyp]
  (let [g (:graph workspace)]
    (some (fn [explained] (= 1 (count (incoming g explained)))) (neighbors g hyp))))

(defn find-conflicts
  "The conflicts of a hyp are those other hyps that share the end of
   some explains link."
  ([workspace hyp]
     (let [g (:graph workspace)]
       (set (filter #(not= % hyp) (mapcat #(incoming g %) (neighbors g hyp))))))
  ([workspace hyp hyps]
     (let [g (:graph workspace)]
       (set/intersection (set hyps) (set (find-conflicts workspace hyp))))))

(defn dot-format
  [workspace boxed best]
  (let [id #(format "%s %s" (:id %) (confidence-str (hyp-conf workspace %)))
        acc (concat (:accepted workspace) (:forced workspace))
        rej (concat (:rejected workspace))
        unexplained (find-unexplained workspace)]
    (str
     "digraph G {\n"
     "rankdir=\"LR\";\n"
     "node [shape=\"plaintext\"];\n"
     (apply str (for [h (nodes (:graph-static workspace))]
                  (apply str (format "\"%s\";\n" (id h))
                         (map #(format "\"%s\" -> \"%s\";\n" (id h) (id %))
                              (:explains h)))))
     (apply str "" (for [h acc]
                     (format "\"%s\" [color=\"blue\", fontcolor=\"blue\"];\n" (id h))))
     (apply str "" (for [h rej]
                     (format "\"%s\" [color=\"red\", fontcolor=\"red\"];\n" (id h))))
     (apply str "" (for [h unexplained]
                     (format "\"%s\" [color=\"orange\", fontcolor=\"orange\"];\n" (id h))))
     "subgraph cluster {\n"
     (apply str "" (map #(format "\"%s\";\n" (id %)) boxed))
     "}\n"
     (if (nil? best) ""
         (format "\"%s\" [color=\"blue\", fontcolor=\"blue\", shape=\"box\"];"
                 (id best)))
     (if (nil? best) ""
         (let [conflicts (find-conflicts workspace best)]
           (apply str ""
                  (map #(format "\"%s\" -> \"%s\" [arrowhead=\"box\"; color=\"red\"];\n"
                                (id acc) (id %)) conflicts))))
     "}\n")))

(defn log
  [workspace hyp-ids msg]
  (update-in workspace [:abducer-log] conj (AbducerLogEntry. hyp-ids msg)))

(defn log-final
  [workspace]
  (let [ids-to-str
        (fn [t] (apply str (interpose ", " (sort (map :id (t workspace))))))]
    (log
     workspace (concat (:accepted workspace) (:rejected workspace))
     (format "Final accepted: %s\n\tFinal rejected: %s\n\tFinal unexplained: %s"
             (ids-to-str :accepted) (ids-to-str :rejected)
             (apply str (interpose ", " (sort (map :id (find-unexplained workspace)))))))))

(defn measure-conf
  [workspace]
  ;; if no accepted hyps, this is very implausible
  (if (empty? (:accepted workspace)) VERY-IMPLAUSIBLE
      ;; if accepted hyps exist, find the minimum confidence of them
      (let [conf (apply min (vals (select-keys (:hyp-confidences workspace)
                                               (:accepted workspace))))]
        ;; if something is unexplained, give worst confidence score
        (if (empty? (find-unexplained workspace)) conf VERY-IMPLAUSIBLE))))

(defn update-conf
  [workspace]
  (assoc-in workspace [:confidence] (measure-conf workspace)))

(defn get-conf
  [workspace]
  (:confidence workspace))

(defn add-hyp
  [workspace hyp]
  (-> workspace
      (update-in [:graph-static] #(apply add-nodes % (conj (:explains hyp) hyp)))
      (update-in [:graph-static] #(apply add-edges % (map (fn [e] [hyp e]) (:explains hyp))))
      (update-in [:hyp-confidences] assoc hyp (:apriori hyp))
      (log [(:id hyp)]
           (format "Adding hypothesis; apriori=%s; explains: %s"
                   (confidence-str (:apriori hyp))
                   (apply str (interpose ","  (map :id (:explains hyp))))))))

(defn reject-many
  [workspace hyps log-msg]
  (-> workspace
      (update-in [:graph] remove-nodes hyps)
      (update-in [:hyp-confidences] #(reduce (fn [c h] (assoc c h IMPOSSIBLE)) % hyps))
      (update-in [:rejected] concat hyps)
      (log (map :id hyps) log-msg)))

(defn accept
  [workspace hyp]
  (let [g (:graph workspace)
        conflicts (find-conflicts workspace hyp)
        removable (concat (neighbors g hyp) (if (empty? (incoming g hyp)) [hyp] []))]
    ;; don't make log entry for acceptance; (explain) does that
    (-> workspace
        (update-in [:accepted] conj hyp)
        (update-in [:graph] #(apply remove-nodes % removable))
        (reject-many conflicts (format "Rejecting because accepting %s." (:id hyp))))))

(defn forced
  [workspace hyp]
  (-> workspace
      (update-in [:forced] conj hyp)
      (log [(:id hyp)] (format "Forcing acceptance of %s." (:id hyp)))))

(defn reset-confidences
  [workspace]
  (assoc workspace :hyp-confidences
         (apply assoc {} (mapcat (fn [h] [h (:apriori h)]) (get-hyps workspace)))))

(defn prepare-workspace
  "Clear the decision, except for what was 'forced'."
  [workspace]
  (assoc workspace :confidence nil :accepted [] :rejected []
         :graph (:graph-static workspace)))

(defn find-best
  [workspace]
  (let [explainers (find-explainers workspace)
        essentials (filter (partial essential? workspace) explainers)
        good-es (filter #(empty? (find-conflicts workspace % essentials)) essentials)]
    (if (not-empty good-es)
      ;; choose first most-confident non-conflicting essential
      (let [best (first (sort-by-conf workspace good-es))]
        {:hyp best :dot (dot-format workspace good-es best)})

      ;; otherwise, ...
      (let [best (first (sort-by-conf workspace explainers))]
        {:hyp best :dot (dot-format workspace explainers best)}))))

(defn explain
  [workspace]
  (loop [ws (-> (prepare-workspace workspace)
                (assoc-in [:resources :hyp-count]
                          (count (nodes (:graph-static workspace)))))]
    (let [best (find-best ws)]
      (println "best" (if best (:id (:hyp best))))
      (println "graph nodes" (if (:graph ws) (map :id (nodes (:graph ws)))))
      (println "graph-static nodes" (map :id (nodes (:graph-static ws))))
      (if (or (empty? (edges (:graph ws))) (nil? best))
        (-> ws
            (update-in [:dot] conj (dot-format ws [] nil))
            (update-conf)
            (log-final))
        (let [explains (neighbors (:graph ws) (:hyp best))]
          (recur
           (-> ws
               (update-in [:dot] conj (:dot best))
               (update-in [:resources :explain-cycles] inc)
               (log (conj (map :id explains) (:id (:hyp best)))
                    (format "Accepting %s as explainer of %s." (:id (:hyp best))
                            (apply str (interpose ", " (map :id explains)))))
               (accept (:hyp best)))))))))
