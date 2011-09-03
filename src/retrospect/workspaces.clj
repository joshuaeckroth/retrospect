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
  [prefix type conflict apriori desc data]
  (let [id (inc last-id)]
    ;; use var-set if running batch mode; def if using player or repl
    ;; (in batch mode, thread is called something like pool-2-thread-1)
    (if (or (= "AWT-EventQueue-0" (. (Thread/currentThread) getName))
            (= "Thread-1" (. (Thread/currentThread) getName))) 
      (def last-id (inc last-id))
      (var-set (var last-id) (inc last-id)))
    {:id (format "%s%d" (if meta? (str "M" prefix) prefix) id)
     :type type
     :conflict conflict
     :apriori apriori :desc desc :data data}))

(defn init-workspace
  ([]
     {:graph nil
      :graph-static (digraph)
      :hyp-confidences {}
      :log {:added [] :forced [] :best [] :accrej {}
            :final {:accepted [] :rejected [] :shared-explains []
                    :unexplained [] :no-explainers [] :unaccepted []}
            :doubt nil}
      :hyp-log {}
      :dot []
      :doubt nil
      :accepted #{}
      :rejected #{}
      :forced #{}
      :resources {:explain-cycles 0 :hypothesis-count 0}})
  ([workspace-old]
   (-> (init-workspace)
     (assoc-in [:resources :explain-cycles]
               (:explain-cycles (:resources workspace-old)))
     (assoc-in [:resources :hypothesis-count]
               (:hypothesis-count (:resources workspace-old))))))

(defn hyp-log
  [workspace hyp]
  (get (:hyp-log workspace) hyp))

(defn get-hyps
  [workspace]
  (nodes (:graph-static workspace)))

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
  (doall (reverse (sort-by (partial hyp-conf workspace) hyps))))

(defn find-explainers
  "Given no hyp argument, returns a sequence of sequences containing
   alternative explainers of hyps needing explanation (essentials are
   therefore seqs with one explainer). A hyp needing explanation is
   one that has been forced and remains in the graph or a hyp in the
   graph that has incoming edges (same criteria as (find-unexplained)
   above). The explainers are sorted by conf (best first), and the seq
   of seqs is sorted by difference between first- and second-best
   alternatives, so that first seq of alts in seq of seqs has greatest
   difference."
  ([workspace]
     (let [g (:graph workspace)]
       (reverse
         (sort-by #(if (second %)
                     (- (hyp-conf workspace (first %))
                        (hyp-conf workspace (second %)))
                     (hyp-conf workspace (first %)))
                  (filter #(> (count %) 1)
                          (map #(sort-by-conf workspace (incoming g %))
                               (find-unexplained workspace)))))))
  ([workspace hyp & opts]
     (let [g (if (some #{:static} opts)
               (:graph-static workspace)
               (:graph workspace))]
       (incoming g hyp))))

(defn normalize-confidences
  "Normalize the apriori confidences of a collection of hyps.
   Returns a map with hyps as keys and new conf's as values."
  [hyps]
  (let [sum (reduce + 0.0 (map #(:apriori %) hyps))]
    (if (= 0.0 sum) {}
      (reduce #(assoc %1 %2 (/ (:apriori %2) sum)) {} hyps))))

(defn update-confidences
  "Update confidences of hyps based on their normalized apriori confidences;
   if a normalized apriori confidence is better than the recorded confidence,
   update the recorded confidence."
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
      (fn? c) (set (filter #(and (not= % hyp) (c hyp %)) hyps))

      ;; :shared-explains conflict id; may conflict with other hyps
      ;; that have :shared-explains id
      (= c :shared-explains)
      (let [other-hyps (filter #(and (not= % hyp)
                                     (= :shared-explains (:conflict %)))
                               hyps)]
        ;; :shared-explains hyps conflict if they shared an explains
        ;; link (neighbor)
        (set (filter #(not-empty (set/intersection (neighbors g %)
                                                   (neighbors g hyp)))
                     other-hyps)))
      ;; otherwise, hyps conflict if their conflict ids are identical
      :else
      (set (filter #(and (not= % hyp) (= c (:conflict %))) hyps)))))

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
   non-static) graph, and if it does not conflict with an accepted or
   forced hyp. However, if the hyp is to be added, add all its explain
   edges to the static graph, but only those unexplained edges to the
   live graph."
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
        (let [ws
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
                  (update-in [:log :added] conj {:hyp hyp :explains expl}))
              conflicts (find-conflicts ws hyp :static)]
          ;; abort if added hyp conflicts with something accepted or forced
          (if (or (not-empty (set/intersection conflicts (:accepted ws)))
                  (not-empty (set/intersection conflicts (:forced ws))))
            workspace ws)))))

(defn reject-many
  [workspace hyps this-cycle]
  (let [rejectable (set/difference (set hyps) (:accepted workspace))]
    (-> (reduce (fn [ws hyp] (update-in ws [:hyp-log hyp] conj
                                      (format "Rejected in cycle %d" this-cycle)))
                  workspace rejectable)
        (update-in [:graph] #(apply remove-nodes % rejectable))
        (update-in [:rejected] set/union (set rejectable))
        (update-in [:log :final :rejected] concat rejectable))))

(defn accept
  [workspace hyp this-cycle inconsistent pdata]
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
                                this-cycle
                                (apply str (interpose ", " (sort (map :id removable))))))
             (update-in [:accepted] conj hyp)
             (assoc :graph new-g))]
    (loop [ws (reject-many ws conflicts this-cycle)
           rejected #{}]
      (let [incon (set/difference
                   (set (inconsistent
                         pdata (set/union (:accepted ws)
                                          (apply concat (find-explainers ws)))
                         (:rejected ws)))
                   rejected)]
        (if (not-empty incon)
          (recur (reject-many ws incon this-cycle) (set/union rejected (set incon)))
          (update-in ws [:log :accrej this-cycle] conj
                     {:acc hyp :rej (concat conflicts rejected)}))))))

(defn transitive-accept
  "Need to accept the transitive explainer last (so what it explains
   can be accepted first and remove what they explain)."
  [workspace hyp this-cycle inconsistent pdata]
  (loop [acceptable [hyp]
         ws workspace]
    (if (empty? acceptable) ws
        (let [accept-later (first acceptable)
              explained (set/difference (find-explains ws accept-later)
                                        (:forced ws) (:accepted ws))]
          (if (empty? explained)
            ;; nothing else to push to the front of the queue, so accept 'accept-later' hyp
            (recur (rest acceptable) (accept ws accept-later this-cycle inconsistent pdata))
            ;; otherwise, have something to accept first; put in front of queue
            (recur (concat explained acceptable) ws))))))

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
  (let [ws (assoc workspace :doubt nil :accepted #{} :rejected #{}
                  :graph (:graph-static workspace))]
    (-> ws
        (assoc-in [:log :best] [])
        (assoc-in [:log :accrej] {})
        (update-in [:resources :hypothesis-count]
                   + (count (nodes (:graph-static ws)))))))

(defn measure-doubt
  [workspace]
  (let [final (:final (:log workspace))]
    (if (empty? (:accepted final)) 
      (if (empty? (:unexplained final)) 0.0 1.0)
      (let [confs (vals (select-keys (:hyp-confidences workspace) (:accepted final)))
            doubt-unexpl (concat (map #(- 1.0 %) confs)
                                 (repeat (count (:unexplained final)) 1.0))]
        (double (/ (reduce + 0.0 doubt-unexpl) (count doubt-unexpl)))))))

(comment (apply max (map #(- 1.0 %) confs)))

(defn get-doubt
  [workspace]
  (:doubt (:log workspace)))

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
                                   (:rejected workspace)
                                   (:forced workspace))})]
    (assoc-in ws [:log :doubt] (measure-doubt ws))))

(defn find-best
  [workspace explainers threshold]
  (let [essentials (apply concat (filter #(= 1 (count %)) explainers))]
    (if (not-empty essentials)
      ;; choose first most-confident essential
      (let [best (first essentials)]
        {:best best :alts (disj (set essentials) best)
         :essential? true :delta nil})
      ;; otherwise, choose highest-delta non-essential
      (let [alts (first explainers)
            best (first alts)
            delta (- (hyp-conf workspace (first alts))
                     (hyp-conf workspace (second alts)))]
        (if (>= delta threshold)
          {:best best :alts (rest alts)
           :essential? false :delta delta})))))

(defn explain
  [workspace inconsistent pdata threshold]
  (loop [ws workspace
         this-cycle 1]
    (if (empty? (edges (:graph ws))) (log-final ws)
        (let [explainers (find-explainers ws)]
          (if (empty? explainers) (log-final ws)
              (let [ws-confs (update-confidences ws explainers)
                    explainers-updated (find-explainers ws-confs)
                    {:keys [best alts essential? delta]}
                    (find-best ws-confs explainers-updated threshold)]
                (if-not best
                  (log-final ws-confs)
                  (recur (-> ws-confs
                             (update-in [:resources :explain-cycles] inc)
                             (update-in [:log :best] conj
                                        {:best best :alts alts
                                         :essential? essential? :delta delta})
                             (transitive-accept best this-cycle inconsistent pdata))
                         (inc this-cycle)))))))))
