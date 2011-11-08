(ns retrospect.workspaces
  (:import (misc AlphanumComparator))
  (:use [retrospect.confidences])
  (:require [clojure.set :as set])
  (:use [loom.io :only [dot-str]])
  (:use [loom.graph :only
         [digraph nodes incoming neighbors weight
          add-nodes add-edges remove-nodes edges transpose]])
  (:use [loom.alg :only [pre-traverse]])
  (:use [loom.attr :only [add-attr remove-attr]])
  (:use [retrospect.state]))

(def last-id 0)

(def meta? false)

(defn set-last-id
  [n]
  (def last-id n))

(defrecord Hypothesis
    [id type conflict apriori explains desc data]
  Object
  (toString [self] (format "%s: %s" id desc)))

(defn new-hyp
  [prefix type conflict apriori explains desc data]
  (let [id (inc last-id)]
    ;; use var-set if running batch mode; def if using player or repl
    ;; (in batch mode, thread is called something like pool-2-thread-1)
    (if (or (= "AWT-EventQueue-0" (. (Thread/currentThread) getName))
            (= "Thread-1" (. (Thread/currentThread) getName))) 
      (def last-id (inc last-id))
      (var-set (var last-id) (inc last-id)))
    (Hypothesis. (format "%s%d" (if meta? (str "M" prefix) prefix) id)
                 type conflict apriori explains desc data)))

(defn init-workspace
  ([]
     {:graph nil
      :graph-static (digraph)
      :cycle 0
      :hyp-confidences {}
      :log {:added [] :forced [] :best [] :accrej {}
            :final {:accepted [] :rejected [] :shared-explains []
                    :unexplained [] :no-explainers [] :unaccepted []}
            :doubt nil}
      :hyp-log {}
      :dot []
      :doubt nil
      :accepted #{}
      :forced #{}
      :rejected #{}
      :resources {:explain-cycles 0 :hypothesis-count 0}}))

(defn hyp-log
  [workspace hyp]
  (get (:hyp-log workspace) hyp))

(defn get-hyps
  [workspace & opts]
  (if (some #{:static} opts)
    (nodes (:graph-static workspace))
    (nodes (:graph workspace))))

(defn find-explains
  [workspace hyp & opts]
  (if (some #{:static} opts)
    (neighbors (:graph-static workspace) hyp)
    (neighbors (:graph workspace) hyp)))

(defn find-explains-transitive-paths
  [workspace hyp & opts]
  (let [g (if (some #{:static} opts)
            (:graph-static workspace)
            (:graph workspace))]
    (loop [paths (map (fn [h] [h]) (neighbors g hyp))]
      (if (empty? (mapcat (fn [path] (neighbors g (last path))) paths))
        paths
        (let [new-paths (mapcat (fn [path] (map (fn [h] (conj path h))
                                                (neighbors g (last path))))
                                paths)]
          (recur new-paths))))))

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

;; TODO: fix
(defn find-unexplained
  "Unexplained hyps are those that are accepted and remain in the graph."
  [workspace]
  (let [g (:graph workspace)
        hyps (nodes g)]
    (set/intersection (:accepted workspace) (set hyps))))

;; TODO: fix
(defn find-no-explainers
  [workspace]
  (let [g (:graph-static workspace)]
    (set (filter #(empty? (incoming g %)) (:forced workspace)))))

(defn hyp-conf
  [workspace hyp]
  (get (:hyp-confidences workspace) hyp))

(defn sort-by-conf
  "Since we are using probabilities, smaller value = less confidence. We want
   most confident first."
  [workspace hyps]
  (reverse (sort-by (partial hyp-conf workspace) hyps)))

(defn sort-by-delta
  [workspace hyps1 hyps2]
  (let [delta-fn (fn [hyps] (if (second hyps)
                              (- (hyp-conf workspace (first hyps))
                                 (hyp-conf workspace (second hyps)))
                              (hyp-conf workspace (first hyps))))
        hyps1-delta (delta-fn hyps1)
        hyps2-delta (delta-fn hyps2)]
    (if (= 0 (compare hyps1-delta hyps2-delta))
      (compare (hyp-conf workspace (first hyps1))
               (hyp-conf workspace (first hyps2)))
      (compare hyps1-delta hyps2-delta))))

(defn incoming-transitive
  "Find transitive explainers of some hypothesis. Immediate explainers
   are not included. Presumably this function is only used when
   transitive-explanation is active."
  [graph hyp & opts]
  (let [tg (transpose graph)]
    ;; use (rest) to remove hyp itself from the list
    (mapcat #(rest (pre-traverse tg %)) (neighbors tg hyp))))

(defn find-all-explainers
  "Returns two sequences of sequences containing alternative
   explainers of hyps needing explanation (essentials are therefore
   seqs with one explainer). A hyp needing explanation is defined
   by (find-unexplained), above, which we call here. The explainers
   are sorted by conf (best first), and the seq of seqs is sorted by
   difference between first- and second-best alternatives, so that
   first seq of alts in seq of seqs has greatest difference. If trans?
   is false, then a hyp is an explainer only if everything it explains
   has already been accepted."
  [workspace trans?]
  (let [g (:graph workspace)
        all-acc #(every? (:accepted workspace) (neighbors g %))
        explainers (map #(sort-by-conf workspace (if trans? (incoming-transitive g %)
                                                     (incoming g %)))
                        (find-unexplained workspace))]
    (reverse (sort (partial sort-by-delta workspace)
                   (filter second
                           (if trans? explainers
                               (map #(filter all-acc %) explainers)))))))

(defn find-explainers
  [workspace hyp & opts]
  (let [g (if (some #{:static} opts)
            (:graph-static workspace)
            (:graph workspace))]
    (if (:TransitiveExplanation params)
      (set/union (incoming g hyp) (incoming-transitive g hyp))
      (incoming g hyp))))

(defn normalize-confidences
  "Normalize the apriori confidences of a collection of hyps.
   Returns a map with hyps as keys and new conf's as values."
  [hyps]
  (let [sum (reduce + 0.0 (map #(:apriori %) hyps))]
    (if (= 0.0 sum) {}
      (reduce #(assoc %1 %2 (/ (:apriori %2) sum)) {} hyps))))

(defn update-confidences
  "Update confidences of hyps based on their normalized apriori
   confidences; if a normalized apriori confidence is better than the
   recorded confidence, update the recorded confidence. This function
   should only be called on a non-transitive (i.e. immediate)
   explanation seq."
  [workspace explainers]
  ;; for each seq of explainers
  (reduce (fn [ws alts]
            (let [norm-alts (normalize-confidences alts)]
              ;; for each normalized confidence hyp
              (reduce (fn [ws2 hyp]
                        ;; if the normalized confidence is greater than
                        ;; the existing confidence, then update it
                        (if (<= (norm-alts hyp) (hyp-conf ws hyp)) ws2
                          (assoc-in ws2 [:hyp-confidences hyp] (norm-alts hyp))))
                      ws (keys norm-alts))))
          workspace explainers))

(defn find-conflicts
  "If a hypothesis's :conflict value is a function (a predicate), that function
   is called with the hyp and each other hyp. If the :conflict value is
   a keyword, the conflicts of a hyp are those other hyps that share the
   :conflict keyword, except if the keyword is :shared-explains. In that case,
   the hyp conflicts with any other that also indicates :shared-explains and
   shares at least one explainer."
  [workspace hyp & opts]
  (let [g (if (some #{:static} opts)
            (:graph-static workspace)
            (:graph workspace))
        ;; a hyp can't conflict with what it explains and what
        ;; explains it, so remove those hyps first
        hyps (set/difference (nodes g)
                             (apply find-explainers workspace hyp opts)
                             (apply find-explains workspace hyp opts))
        c (:conflict hyp)]
    (cond
      ;; no conflict id; so it conflicts with nothing
      (nil? c) []

      ;; we have a function (predicate), so call the function on other hyps
      (fn? c) (filter #(and (not= % hyp) (c hyp %)) hyps)

      ;; :shared-explains conflict id; may conflict with other hyps
      ;; that have :shared-explains id
      (= c :shared-explains)
      (let [other-hyps (filter #(and (not= % hyp)
                                     (= :shared-explains (:conflict %)))
                               hyps)]
        ;; :shared-explains hyps conflict if they shared an explains
        ;; link (neighbor)
        (filter #(not-empty (set/intersection (neighbors g %)
                                              (neighbors g hyp)))
                other-hyps))
      ;; otherwise, hyps conflict if their conflict ids are identical
      :else
      (filter #(and (not= % hyp) (= c (:conflict %))) hyps))))

(defn add
  [workspace hyp & opts]
  (let [gtype (if (some #{:static} opts) :graph-static :graph)
        ns (nodes (get workspace gtype))
        ;; expl has only those hyps that are explained and currently reside in the graph
        expl (filter ns (:explains hyp))
        ;; is this hyp explaining something in the graph? (or is it :graph-static?)
        explains-something-unexplained? (or (= gtype :graph-static) (not-empty expl))]
    (if (not explains-something-unexplained?) workspace
        ;; add to :graph-static if :static was not indicated;
        ;; here, add even already-explains links
        (let [ws-static
              (if (not= gtype :graph) workspace
                  (let [g-added (reduce (fn [g n] (-> g (add-nodes n)
                                                      (add-attr n :id (:id n))
                                                      (add-attr n :label (:id n))))
                                        (:graph-static workspace)
                                        (conj (:explains hyp) hyp))
                        g-edges (reduce (fn [g e] (add-edges g [hyp e]))
                                        g-added (:explains hyp))]
                    (assoc workspace :graph-static g-edges)))
              g-added (reduce (fn [g n] (-> g (add-nodes n)
                                            (add-attr n :id (:id n))
                                            (add-attr n :label (:id n))))
                              (gtype ws-static) (conj expl hyp))
              g-edges (reduce (fn [g e] (add-edges g [hyp e])) g-added expl)
              ws-updated (-> ws-static
                             (assoc gtype g-edges)
                             (assoc-in [:hyp-confidences hyp] (:apriori hyp))
                             (update-in [:log :added] conj {:hyp hyp :explains expl}))
              conflicts (find-conflicts ws-updated hyp :static)]
          ;; abort if added hyp conflicts with something accepted
          (if (some (:accepted ws-updated) conflicts)
            workspace ws-updated)))))

(defn reject-many
  [workspace hyps]
  (let [accepted (:accepted workspace)
        rejectable (filter #(not (accepted %)) hyps)]
    (-> (reduce (fn [ws hyp]
                  (update-in ws [:hyp-log hyp] conj
                             (format "Rejected in cycle %d" (:cycle workspace))))
                workspace rejectable)
        (update-in [:graph-static]
                   #(reduce (fn [g r] (-> g (add-attr r :fontcolor "red")
                                          (add-attr r :color "red")))
                            % rejectable))
        (update-in [:graph] #(apply remove-nodes % rejectable))
        (update-in [:rejected] set/union (set rejectable))
        (update-in [:log :final :rejected] concat rejectable))))

(defn accept
  [workspace hyp pdata]
  (let [g (:graph workspace)
        removable (concat (neighbors g hyp)
                          (if (empty? (incoming g hyp)) [hyp] []))
        ;; must find conflicts in original workspace, before explained
        ;; hyps are removed
        conflicts (find-conflicts workspace hyp)
        new-g (apply remove-nodes g removable)
        ws (-> workspace
             (update-in [:hyp-log hyp] conj
                        (format "Accepted in cycle %d (removed %s)"
                                (:cycle workspace)
                                (apply str (interpose ", " (sort (map :id removable))))))
             (update-in [:accepted] conj hyp)
             (update-in [:graph-static] add-attr hyp :fontcolor "green")
             (update-in [:graph-static] add-attr hyp :color "green")
             (assoc :graph new-g))]
    (loop [ws (reject-many ws conflicts)
           rejected #{}]
      (let [hyps (set/union (:accepted ws)
                            (apply concat (find-all-explainers ws false)))
            incon (set/difference (set ((:inconsistent-fn @problem) pdata hyps (:rejected ws)))
                                  rejected)]
        (if (not-empty incon)
          (recur (reject-many ws incon) (set/union rejected (set incon)))
          (update-in ws [:log :accrej (:cycle workspace)] conj
                     {:acc hyp :rej (concat conflicts rejected)}))))))

(defn transitive-accept
  "Need to accept the transitive explainer last so that any lower,
   accepted hypotheses remove the appropriate hypotheses that they
   explain. A hypothesis is only accepted via transitive explanation
   if that hypothesis is a member of every path from the explained hyp
   and the accepted hyp."
  [workspace hyp pdata]
  (let [paths (find-explains-transitive-paths workspace hyp)
        acceptable (conj (cond (empty? paths) []
                               (= 1 (count paths)) (first paths)
                               :else (apply set/intersection (map set paths))) hyp)]
    (reduce (fn [ws h] (accept ws h pdata)) workspace acceptable)))

(defn force-accept
  [workspace hyp]
  (-> workspace
      (update-in [:forced] conj hyp)
      (update-in [:log :forced] conj hyp)
      (update-in [:accepted] conj hyp)
      (update-in [:log :accepted] conj hyp)))

(defn reset-confidences
  [workspace]
  (let [hyps (get-hyps workspace)]
    (if (empty? hyps) workspace
        (assoc workspace :hyp-confidences
               (apply assoc {} (mapcat (fn [h] [h (:apriori h)]) hyps))))))

(defn prepare-workspace
  "Clear the decision, except for what was 'forced'."
  [workspace]
  (let [ws (assoc workspace :doubt nil :accepted #{} :rejected #{}
                  :graph (:graph-static workspace))]
    (-> ws
        (update-in [:graph-static]
                   #(reduce (fn [g h] (-> g (remove-attr h :fontcolor)
                                          (remove-attr h :color)))
                            % (nodes %)))
        (assoc-in [:log :best] [])
        (assoc-in [:log :accrej] {})
        (assoc-in [:log :accepted] (:forced workspace))
        (assoc-in [:accepted] (:forced workspace))
        (update-in [:resources :hypothesis-count]
                   + (count (nodes (:graph-static ws)))))))

(defn measure-doubt
  [workspace]
  (let [final (:final (:log workspace))]
    (if (empty? (:accepted final)) 
      (if (empty? (:unexplained final)) 0.0 1.0)
      (let [confs (vals (select-keys (:hyp-confidences workspace) (:accepted final)))]
        (double (/ (reduce + 0.0 (map #(- 1.0 %) confs)) (count confs)))))))

(defn get-doubt
  [workspace]
  (:doubt (:log workspace)))

(defn measure-unexplained-pct
  [workspace]
  (if (empty? (:accepted workspace)) 0.0
      (double (/ (count (:unexplained (:final (:log workspace))))
                 (count (:accepted workspace))))))

(defn get-unexplained-pct
  [workspace]
  (:unexplained-pct (:log workspace)))

(defn log-final
  [workspace]
  (let [ws (assoc-in workspace [:log :final]
                     {:accepted (:accepted workspace)
                      :rejected (:rejected workspace)
                      :shared-explains (find-shared-explains workspace)
                      :unexplained (find-unexplained workspace)
                      :no-explainers (find-no-explainers workspace)
                      :unaccepted (set/difference
                                   (get-hyps workspace)
                                   (:accepted workspace)
                                   (:rejected workspace))})
        ws2 (assoc-in ws [:log :unexplained-pct] (measure-unexplained-pct ws))]
    (assoc-in ws2 [:log :doubt] (measure-doubt ws2))))

(defn find-best
  [workspace explainers threshold]
  (if (empty? explainers) {}
      (let [essentials (apply concat (filter #(= 1 (count %)) explainers))]
        (if (not-empty essentials)
          ;; choose first most-confident essential
          (let [best (first essentials)]
            {:best best :alts (filter #(not= % best) essentials)
             :essential? true :delta nil})
          ;; otherwise, choose highest-delta non-essential
          (let [alts (first explainers)
                best (first alts)
                delta (- (hyp-conf workspace (first alts))
                         (hyp-conf workspace (second alts)))]
            (if (>= delta threshold)
              {:best best :alts (rest alts)
               :essential? false :delta delta}))))))

(defn find-best-multi
  [workspace immediate-explainers transitive-explainers threshold]
  (let [best-immediate
        (assoc (find-best workspace immediate-explainers threshold) :transitive? false)
        best-transitive
        (assoc (find-best workspace transitive-explainers threshold) :transitive? true)]
    (cond
     (not (:TransitiveExplanation params)) best-immediate
     (:essential? best-immediate) best-immediate
     (:essential? best-transitive) best-transitive
     (not (:best best-immediate)) best-transitive
     (not (:best best-transitive)) best-immediate
     (> (:delta best-immediate) (:delta best-transitive)) best-immediate
     :else best-transitive)))

(defn explain
  [workspace pdata]
  (loop [ws workspace]
    (if (empty? (edges (:graph ws))) (log-final ws)
        (let [immediate-explainers (find-all-explainers ws false)
              transitive-explainers (if (:TransitiveExplanation params)
                                      (find-all-explainers ws true) [])]
          (if (or (and (empty? immediate-explainers)
                       (not (:TransitiveExplanation params)))
                  (and (empty? immediate-explainers) (empty? transitive-explainers)))
            (log-final ws)
            (let [ws-confs (update-confidences ws immediate-explainers)
                  immediate-explainers-updated (find-all-explainers ws-confs false)
                  transitive-explainers-updated (find-all-explainers ws-confs true)
                  {:keys [best alts essential? transitive? delta]}
                  (find-best-multi ws-confs
                                   immediate-explainers-updated
                                   transitive-explainers-updated
                                   (/ (:Threshold params) 100.0))]
              (if-not best
                (log-final ws-confs)
                (recur
                 (let [ws-logged (-> ws-confs
                                     (update-in [:cycle] inc)
                                     (update-in [:resources :explain-cycles] inc)
                                     (update-in [:log :best] conj
                                                {:best best :alts alts
                                                 :essential? essential?
                                                 :transitive? transitive? :delta delta}))]
                   (if transitive?
                     (transitive-accept ws-logged best pdata)
                     (accept ws-logged best pdata)))))))))))
