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
            :final {:accepted [] :rejected [] :shared-explains [] :unexplained []}
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
        only-best-conf (take-while #(= best-conf (hyp-conf workspace %)) sorted)]
    (first (sort-by :id (AlphanumComparator.) only-best-conf))))

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

(defn find-shared-explains
  [workspace]
  (let [hyps (:accepted workspace)]
    (if (>= 1 (count hyps)) []
        (filter (fn [hyp] (not-empty
                           (apply set/union
                                  (map #(set/intersection
                                         (find-explains workspace hyp :static)
                                         (find-explains workspace % :static))
                                       (disj hyps hyp)))))
                hyps))))

(defn find-unexplained
  "Unexplained hyps are those that are forced and remain in the graph,
   and those that are in the graph and have incoming edges."
  [workspace]
  (let [g (:graph workspace)
        hyps (nodes g)]
    (set (concat (set/intersection (set (:forced workspace)) (set hyps))
                 (filter #(not-empty (incoming g %)) hyps)))))

(defn essential?
  [workspace hyp]
  (let [g (:graph workspace)]
    (some (fn [explained] (= 1 (count (incoming g explained)))) (neighbors g hyp))))

(defn find-conflicts
  "The conflicts of a hyp are those other hyps that share the end of
   some explains link, or that share a :conflict-id."
  [workspace hyp & opts]
  []
  (comment (let [g (if (some #{:static} opts) (:graph-static workspace) (:graph workspace))]
             (set (concat
                   (filter #(and (not= % hyp)
                                 (not (nil? (:conflict-id hyp)))
                                 (= (:conflict-id hyp) (:conflict-id %)))
                           (nodes g))
                   (filter #(not= % hyp) (mapcat #(incoming g %) (neighbors g hyp))))))))

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
  (assoc workspace :confidence (measure-conf workspace)))

(defn get-conf
  [workspace]
  (:confidence workspace))

(defn add
  "Only adds edges for hyps that it explains if those explained hyps
   are already in the graph."
  [workspace hyp explains]
  (let [expl (set/intersection (nodes (:graph-static workspace)) (set explains))]
    (-> workspace
        (update-in [:graph-static] #(apply add-nodes % (conj expl hyp)))
        (update-in [:graph-static] #(apply add-edges % (map (fn [e] [hyp e]) expl)))
        (update-in [:hyp-confidences] assoc hyp (:apriori hyp))
        (update-in [:log :added] conj {:hypid (:id hyp) :explains (map :id expl)
                                       :apriori (:apriori hyp)}))))

(defn reject-many
  [workspace hyps]
  (-> workspace
      (update-in [:graph] #(apply remove-nodes % hyps))
      (update-in [:hyp-confidences] #(reduce (fn [c h] (assoc c h IMPOSSIBLE)) % hyps))
      (update-in [:rejected] set/union (set hyps))
      (update-in [:log :rejected] concat (map :id hyps))))

(defn accept
  [workspace hyp]
  (let [g (:graph workspace)
        conflicts (find-conflicts workspace hyp)
        removable (concat (neighbors g hyp) (if (empty? (incoming g hyp)) [hyp] []))]
    (-> workspace
        (update-in [:accepted] conj hyp)
        (update-in [:graph] #(apply remove-nodes % removable))
        (update-in [:log :accrej] conj {:acc (:id hyp) :rej (map :id conflicts)})
        (reject-many conflicts))))

(defn forced
  [workspace hyp]
  (-> workspace
      (update-in [:forced] conj hyp)
      (update-in [:log :forced] conj (:id hyp))))

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

(defn log-final
  [workspace]
  (-> workspace
      (assoc-in [:log :final]
                {:accepted (map :id (:accepted workspace))
                 :rejected (map :id (:rejected workspace))
                 :shared-explains (map :id (find-shared-explains workspace))
                 :unexplained (map :id (find-unexplained workspace))})
      (assoc-in [:log :confidence] (:confidence workspace))))

(defn find-best
  [workspace]
  (let [explainers (find-explainers workspace)
        essentials (doall (filter (partial essential? workspace) explainers))
        good-es (doall (filter #(empty? (set/intersection (set essentials)
                                                          (find-conflicts workspace %)))
                               essentials))]
    (if (not-empty good-es)
      ;; choose first most-confident non-conflicting essential
      (let [best (pick-top-conf workspace good-es)]
        {:best best :alts good-es :essential? true})
      ;; otherwise, ...
      (let [best (pick-top-conf workspace explainers)]
        {:best best :alts explainers :essential? false}))))

(defn explain
  [workspace]
  (if (empty? (edges (:graph workspace))) (-> workspace (update-conf) (log-final))
      (let [{best :best alts :alts essential? :essential?} (find-best workspace)]
        (if-not best
          (-> workspace (update-conf) (log-final))
          (recur
           (-> workspace
               (update-in [:resources :explain-cycles] inc)
               (update-in [:log :best] conj
                          {:best (:id best) :alts (map :id alts) :essential? essential?})
               (accept best)))))))
