(ns retrospect.workspaces
  (:import (misc AlphanumComparator))
  (:use [retrospect.confidences])
  (:require [clojure.set :as set])
  (:use [loom.graph :only
         [digraph nodes incoming neighbors weight
          add-nodes add-edges remove-nodes edges]]))

(def last-id 0)

(def meta? false)

(defn set-last-id
  [n]
  (def last-id n))

(defn new-hyp
  [prefix type conflict-id apriori desc data]
  (let [id (inc last-id)]
    ;; use var-set if running batch mode; def if using player
    (if (= "AWT-EventQueue-0" (. (Thread/currentThread) getName))
      (def last-id (inc last-id))
      (var-set (var last-id) (inc last-id)))
    {:id (format "%s%d" (if meta? (str "M" prefix) prefix) id)
     :type type
     :conflict-id conflict-id
     :apriori apriori :desc desc :data data}))

(defn init-workspace
  ([]
     {:graph nil
      :graph-static (digraph)
      :hyp-confidences {}
      :log {:added [] :forced [] :best [] :accrej []
            :final {:accepted [] :rejected [] :shared-explains []
                    :unexplained [] :unaccepted []}
            :confidence nil}
      :dot []
      :confidence nil
      :accepted #{}
      :rejected #{}
      :forced #{}
      :resources {:explain-cycles 0 :hyp-count 0}})
  ([workspace-old]
     (assoc-in (init-workspace) [:resources :explain-cycles]
               (:explain-cycles (:resources workspace-old)))))

(defn get-hyps
  [workspace]
  (nodes (:graph-static workspace)))

(defn find-explainers
  ([workspace]
     (let [g (:graph workspace)]
       (set (mapcat #(incoming g %) (nodes g)))))
  ([workspace hyp & opts]
     (let [g (if (some #{:static} opts)
               (:graph-static workspace)
               (:graph workspace))]
       (incoming g hyp))))

(defn find-explains
  [workspace hyp & opts]
  (if (some #{:static} opts)
    (neighbors (:graph-static workspace) hyp)
    (neighbors (:graph workspace) hyp)))

(defn shared-explains?
  [workspace hyp hyps]
  (let [expl (find-explains workspace hyp :static)]
    (not-empty
     (apply set/union
            (map #(set/intersection expl
                   (find-explains workspace % :static))
                 (disj hyps hyp))))))

(defn find-shared-explains
  [workspace]
  (let [hyps (:accepted workspace)]
    (if (>= 1 (count hyps)) []
        (sort-by :id (AlphanumComparator.)
                 (filter #(shared-explains? workspace % hyps) hyps)))))

(defn find-unexplained
  "Unexplained hyps are those that are forced and remain in the graph,
   and those that are in the graph and have incoming edges."
  [workspace]
  (let [g (:graph workspace)
        hyps (nodes g)]
    (set (concat (set/intersection (set (:forced workspace)) (set hyps))
                 (filter #(not-empty (incoming g %)) hyps)))))

(defn hyp-conf
  [workspace hyp]
  (get (:hyp-confidences workspace) hyp))

(defn sort-by-conf
  "Since we are using logprob, smaller values = more confidence."
  [workspace hyps]
  (doall (sort-by (partial hyp-conf workspace) hyps)))

(defn find-alternatives
  [workspace hyp]
  (let [g (:graph workspace)]
    (disj (set (mapcat (fn [explained] (incoming g explained))
                       (neighbors g hyp)))
          hyp)))

(defn essential?
  [workspace hyp]
  (let [g (:graph workspace)]
    (some (fn [explained] (= 1 (count (incoming g explained))))
          (neighbors g hyp))))

(defn find-conflicts
  "The conflicts of a hyp are those other hyps that share
   a :conflict-id. Some conflict ids are special: :shared-explains
   means this hyp conflicts with any other that both has the
   same :shared-explains conflict id and explains one of the same
   hyps."
  [workspace hyp & opts]
  (let [g (if (some #{:static} opts)
            (:graph-static workspace)
            (:graph workspace))
        hyps (nodes g)
        cid (:conflict-id hyp)]
    (cond
     ;; no conflict id; so it conflicts with nothing
     (nil? cid) []

     ;; :shared-explains conflict id; may conflict with other hyps
     ;; that have :shared-explains id
     (= cid :shared-explains)
     (let [other-hyps (filter #(and (not= % hyp)
                                    (= :shared-explains (:conflict-id %)))
                              hyps)]
       ;; :shared-explains hyps conflict if they shared an explains
       ;; link (neighbor)
       (set (filter #(not-empty (set/intersection (neighbors g %)
                                                  (neighbors g hyp)))
                    other-hyps)))
     ;; otherwise, hyps conflict if their conflict ids are identical
     :else
     (set (filter #(and (not= % hyp) (= cid (:conflict-id %))) hyps)))))

;; TODO: Update for green entity colors
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
                              (find-explains workspace h :static)))))
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

(defn add
  "Only add a hyp if its explained hyps are already in the (live,
   non-static) graph. However, if the hyp is to be added, add all its
   explain edges to the static graph, but only those unexplained edges
   to the live graph."
  [workspace hyp explains & opts]
  (let [gtype (if (some #{:static} opts)
                :graph-static
                :graph)
        expl (set/intersection (nodes (get workspace gtype))
                               (set explains))
        explains-something-unexplained?
        (or (= gtype :graph-static) (not-empty expl))]
    (if (not explains-something-unexplained?) workspace
        ;; add to :graph-static if :static was not indicated;
        ;; here, add even already-explains links
        (-> (if (not= gtype :graph) workspace
                (-> workspace
                    (update-in [:graph-static]
                               #(apply add-nodes % (conj explains hyp)))
                    (update-in [:graph-static]
                               #(apply add-edges % (map (fn [e] [hyp e]) explains)))))
            ;; just add unexplained links
            (update-in [gtype]
                       #(apply add-nodes % (conj expl hyp)))
            (update-in [gtype]
                       #(apply add-edges % (map (fn [e] [hyp e]) expl)))
            (update-in [:hyp-confidences] assoc hyp (:apriori hyp))
            (update-in [:log :added] conj {:hyp hyp :explains expl})))))

(defn reject-many
  [workspace hyps]
  (let [rejectable (set/difference (set hyps) (:accepted workspace))]
    (-> workspace
        (update-in [:graph] #(apply remove-nodes % rejectable))
        (update-in [:hyp-confidences]
                   #(reduce (fn [c h] (assoc c h 100.0)) % rejectable))
        (update-in [:rejected] set/union (set rejectable))
        (update-in [:log :final :rejected] concat rejectable))))

(defn penalize-hyps
  [workspace hyps]
  (update-in workspace [:hyp-confidences]
               #(reduce (fn [c h] (assoc c h (penalize (hyp-conf workspace h))))
                        % hyps)))

(defn accept
  [workspace hyp inconsistent pdata]
  (let [g (:graph workspace)
        removable (concat (neighbors g hyp)
                          (if (empty? (incoming g hyp)) [hyp] []))
        ;; must find conflicts in original workspace, before explained
        ;; hyps are removed
        conflicts (find-conflicts workspace hyp)
        new-g (apply remove-nodes g removable)
        ws (-> workspace
               (update-in [:accepted] conj hyp)
               (assoc :graph new-g))]
    (loop [ws (reject-many ws conflicts)
           rejected #{}]
      (let [incon (set/difference
                   (set (inconsistent
                         pdata (concat (:accepted ws) (find-explainers ws))
                         (:rejected ws)))
                   rejected)]
        (if (not-empty incon)
          (recur (reject-many ws incon) (set/union rejected (set incon)))
          (update-in ws [:log :accrej] conj
                     {:acc hyp :rej (concat conflicts rejected)}))))))

(defn forced
  [workspace hyp]
  (-> workspace
      (update-in [:forced] conj hyp)
      (update-in [:log :forced] conj hyp)))

(defn reset-confidences
  [workspace]
  (let [hyps (get-hyps workspace)]
    (if (empty? hyps) workspace
        (assoc workspace :hyp-confidences
               (apply assoc {} (mapcat (fn [h] [h (:apriori h)]) hyps))))))

(defn prepare-workspace
  "Clear the decision, except for what was 'forced'."
  [workspace]
  (let [ws (assoc workspace :confidence nil :accepted #{} :rejected #{}
                  :graph (:graph-static workspace))]
    (-> ws
        (assoc-in [:log :best] [])
        (assoc-in [:log :accrej] [])
        (assoc-in [:resources :hyp-count] (count (nodes (:graph-static ws)))))))

(defn measure-conf
  [workspace]
  (let [final (:final (:log workspace))]
    (if (empty? (:accepted final)) 
      (if (empty? (:unexplained final)) 0.0 100.0)
      (let [confs (vals (select-keys (:hyp-confidences workspace)
                                     (:accepted final)))]
        (reduce + 0.0 confs)))))

(defn get-conf
  [workspace]
  (:confidence (:log workspace)))

(defn log-final
  [workspace]
  (let [ws (assoc-in workspace [:log :final]
                     {:accepted (:accepted workspace)
                      :rejected (:rejected workspace)
                      :shared-explains (find-shared-explains workspace)
                      :unexplained (find-unexplained workspace)
                      :unaccepted (set/difference
                                   (get-hyps workspace)
                                   (:accepted workspace)
                                   (:rejected workspace)
                                   (:forced workspace))})]
    (assoc-in ws [:log :confidence] (measure-conf ws))))

(defn find-best
  [workspace]
  (let [explainers (find-explainers workspace)
        essentials (set (filter #(essential? workspace %) explainers))]
    (if (not-empty essentials)
      ;; choose first most-confident essential
      (let [best (first (sort-by-conf workspace essentials))]
        {:best best :alts (disj essentials best)
         :essential? true :delta nil})
      ;; otherwise, choose most-confident non-essential
      (let [alts (sort-by-conf workspace explainers)
            best (first alts)
            delta (if (= 1 (count alts)) 0.0
                      (- (hyp-conf workspace best)
                         (hyp-conf workspace (second alts))))]
        {:best best :alts alts :essential? false :delta delta}))))

(defn explain
  [workspace inconsistent pdata]
  (loop [ws workspace]
    (if (empty? (edges (:graph ws))) (log-final ws)
        (let [{:keys [best alts essential? delta]} (find-best ws)]
          (if-not best
            (log-final ws)
            (recur (-> ws
                       (update-in [:resources :explain-cycles] inc)
                       (update-in [:log :best] conj
                                  {:best best :alts alts
                                   :essential? essential? :delta delta})
                       (accept best inconsistent pdata))))))))
