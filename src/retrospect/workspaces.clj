(ns retrospect.workspaces
  (:import (misc AlphanumComparator))
  (:use [retrospect.confidences])
  (:require [clojure.set :as set])
  (:use [loom.graph :only
         [digraph nodes incoming neighbors weight
          add-nodes add-edges remove-nodes edges]]))

(def last-id 0)

(def meta? false)

(defn new-hyp
  [prefix type conflict-id apriori desc data]
  (let [id (inc last-id)]
    ;; use var-set if running batch mode; def if using player
    (if (= "AWT-EventQueue-0" (. (Thread/currentThread) getName))
      (def last-id (inc last-id))
      (var-set (var last-id) (inc last-id)))
    {:id (format "%s%d" (if meta? (str "M" prefix) prefix) id)
     :type type
     ;; if conflict-id is nil, set it to the uniq :id
     :conflict-id (if conflict-id conflict-id id)
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
     (let [g (if (some #{:static} opts) (:graph-static workspace) (:graph workspace))]
       (incoming g hyp))))

(defn find-explains
  [workspace hyp & opts]
  (if (some #{:static} opts)
    (neighbors (:graph-static workspace) hyp)
    (neighbors (:graph workspace) hyp)))

(defn shared-explains?
  [workspace hyp hyps]
  (not-empty
   (apply set/union
          (map #(set/intersection
                 (find-explains workspace hyp :static)
                 (find-explains workspace % :static))
               (disj hyps hyp)))))

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
  [workspace hyps]
  (doall (reverse (sort-by (partial hyp-conf workspace) hyps))))

(defn pick-top-conf
  [workspace hyps]
  (let [sorted (sort-by-conf workspace hyps)
        best-conf (hyp-conf workspace (first sorted))
        only-best-conf (take-while #(= best-conf (hyp-conf workspace %)) sorted)
        max-explains (last (sort (map #(count (find-explains workspace %)) only-best-conf)))
        only-max-explains (filter #(= max-explains (count (find-explains workspace %)))
                                  only-best-conf)]
    (first (sort-by :id (AlphanumComparator.) only-max-explains))))

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
  "The conflicts of a hyp are those other hyps that share a :conflict-id"
  [workspace hyp & opts]
  (if (nil? (:conflict-id hyp)) []
      (let [g (if (some #{:static} opts) (:graph-static workspace) (:graph workspace))]
        (set (filter #(and (not= % hyp) (= (:conflict-id hyp) (:conflict-id %)))
                     (nodes g))))))

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
  "Only adds edges for hyps that it explains if those explained hyps
   are already in the graph."
  [workspace hyp explains]
  (let [expl (set/intersection (nodes (:graph-static workspace)) (set explains))]
    (-> workspace
        (update-in [:graph-static]
                   #(apply add-nodes % (conj expl hyp)))
        (update-in [:graph-static]
                   #(apply add-edges % (map (fn [e] [hyp e]) expl)))
        (update-in [:hyp-confidences] assoc hyp (:apriori hyp))
        (update-in [:log :added] conj {:hyp hyp :explains expl}))))

(defn reject-many
  [workspace hyps]
  (-> workspace
      (update-in [:graph] #(apply remove-nodes % hyps))
      (update-in [:hyp-confidences]
                 #(reduce (fn [c h] (assoc c h IMPOSSIBLE)) % hyps))
      (update-in [:rejected] set/union (set hyps))
      (update-in [:log :rejected] concat hyps)))

(defn reject-inconsistent
  [workspace consistent? pdata]
  (let [inconsistent
        (set (filter #(not (consistent? pdata [%]))
                     (find-explainers workspace)))]
    (reject-many workspace inconsistent)))

(defn penalize-hyps
  [workspace hyps]
  (update-in workspace [:hyp-confidences]
               #(reduce (fn [c h] (assoc c h (penalize (hyp-conf workspace h))))
                        % hyps)))

(defn accept
  [workspace hyp]
  (let [g (:graph workspace)
        conflicts (find-conflicts workspace hyp)
        removable (concat (neighbors g hyp)
                          (if (empty? (incoming g hyp)) [hyp] []))
        ws (-> workspace
               (update-in [:accepted] conj hyp)
               (update-in [:graph] #(apply remove-nodes % removable))
               (reject-many conflicts))]
    (update-in ws [:log :accrej] conj {:acc hyp :rej conflicts})))

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
    (assoc-in ws [:resources :hyp-count] (count (nodes (:graph-static ws))))))

(defn measure-conf
  [workspace]
  ;; if no accepted hyps, this is very implausible
  (if (empty? (:accepted workspace)) VERY-IMPLAUSIBLE
      ;; if accepted hyps exist, find the minimum confidence of them
      (let [conf (apply max (vals (select-keys (:hyp-confidences workspace)
                                               (:accepted workspace))))]
        (cond
         ;; if something is unexplained, give worst confidence score
         (not-empty (find-unexplained workspace)) VERY-IMPLAUSIBLE
         ;; if shared-explains is not empty, penalize confidence
         (not-empty (:shared-explains (:final (:log workspace)))) (penalize conf)
         ;; otherwise go with lowest accepted hypothesis confidence
         :else conf))))

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

(defn filter-delta
  [workspace]
  (let [delta-better? (fn [delta h1 h2] (<= delta (- (hyp-conf workspace h1)
                                                     (hyp-conf workspace h2))))
        unexplained (find-unexplained workspace)
        explainers (set (map #(find-explainers workspace %) unexplained))]
    (loop [delta 5]
      ;; change result when delta=0 to return #{} as hyps to prevent guessing
      (if (= delta 0) {:hyps (set (apply concat explainers)) :delta 0}
          (let [hs (set (mapcat
                         (fn [es] (filter
                                   #(every? (partial delta-better? delta %)
                                            (disj (set es) %))
                                   es))
                         explainers))
                best (set (filter #(not (shared-explains? workspace % hs)) hs))]
            (cond
             (not-empty best) {:hyps best :delta delta}
             (not-empty hs) {:hyps hs :delta delta}
             :else (recur (dec delta))))))))

;; TODO: check if essentials ever (when all added) are inconsistent --
;; this should never happen

;; maybe even quit explaining at delta=1 (don't go to delta=0)
(defn find-best
  [workspace consistent? pdata]
  (let [explainers (find-explainers workspace)
        essentials (set (filter #(essential? workspace %) explainers))]
    (if (not-empty essentials)
      ;; choose first most-confident essential
      (let [best (pick-top-conf workspace essentials)]
        {:best best :alts (disj essentials best)
         :essential? true :delta nil})
      ;; otherwise, choose any clear-best, weak-best, or make a guess (top-conf)
      (let [{alts :hyps delta :delta} (filter-delta workspace)
            best (pick-top-conf workspace alts)]
        ;; check for pairwise inconsistency with best set
        (doseq [h alts]
          (when (some #(not (consistent? pdata [h %]))
                      (disj alts h))
            ;; if this happens, just quit (leave it unexplained)
            (println (:id h) "is consistent with anything else in best = "
                     (map :id alts) "for delta" delta)))
        {:best best :alts (disj alts best)
         :essential? false :delta delta}))))

;; commit decision as you go (would make consistency-checks faster)?
;; yes, but wipe it out before truly committing; we don't care about
;; new labels at consistency-checking time
(defn explain
  [workspace consistent? commit-decision pdata]
  ;; start off by rejecting any inconsistent hyps even before
  ;; we have accepted anything
  (loop [ws (reject-inconsistent workspace consistent? pdata)
         pd pdata]
    (if (empty? (edges (:graph ws))) (log-final ws)
        (let [{:keys [best alts essential? delta]}
              (find-best ws consistent? pd)]
          (if-not best
            (log-final ws)
            (recur (-> ws
                       (update-in [:resources :explain-cycles] inc)
                       (update-in [:log :best] conj
                                  {:best best :alts alts
                                   :essential? essential? :delta delta})
                       (accept best)
                       (reject-inconsistent consistent? pd))
                   (commit-decision pd [best])))))))
