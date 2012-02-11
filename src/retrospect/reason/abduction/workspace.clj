(ns retrospect.reason.abduction.workspace
  (:import (misc AlphanumComparator))
  (:use [retrospect.confidences])
  (:require [clojure.set :as set])
  (:use [loom.io :only [dot-str]])
  (:use [loom.graph :only
         [digraph nodes incoming neighbors weight
          add-nodes add-edges remove-nodes edges transpose]])
  (:use [loom.alg :only [pre-traverse]])
  (:use [loom.attr :only [add-attr remove-attr]])
  (:use [clojure.contrib.combinatorics :only [combinations]])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.state]))

(defrecord Hypothesis
    [id type subtype conflict apriori explains depends desc]
  Object
  (toString [self] (format "%s: %s" id desc))
  Comparable
  (compareTo [self other] (compare (hash self) (hash other))))

(defmethod print-method Hypothesis
  [o w]
  (print-simple (:id o) w))

(comment   (print-simple (str "#<" (:id o) ": \"" (:desc o) "\">") w))

(defn new-hyp
  [prefix type subtype conflict apriori explains depends desc data]
  (let [id (inc last-id)]
    (set-last-id id)
    ;; use var-set if running batch mode; def if using player or repl
    ;; (in batch mode, thread is called something like pool-2-thread-1)

    (merge
     (Hypothesis. (format "%s%d" prefix id)
                  type subtype conflict apriori explains depends desc)
     data)))

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

(defn find-explainers
  [workspace hyp & opts]
  (let [g (if (some #{:static} opts)
            (:graph-static workspace)
            (:graph workspace))]
    (vals (group-by :type (sort-by :id (incoming g hyp))))))

;; TODO: fix
(defn find-no-explainers
  [workspace]
  (let [g (:graph-static workspace)]
    (set (filter #(empty? (incoming g %)) (:forced workspace)))))

(defn hyp-conf
  [workspace hyp]
  (get (:hyp-confidences workspace) hyp))

(defn compare-by-conf
  "Since we are using probabilities, smaller value = less confidence. We want
   most confident first."
  [workspace hyp1 hyp2]
  (if (= 0 (compare (hyp-conf workspace hyp1)
                    (hyp-conf workspace hyp2)))
    (compare (:id hyp1) (:id hyp2))
    (compare (hyp-conf workspace hyp1)
             (hyp-conf workspace hyp2))))

(defn compare-by-delta
  [workspace hyps1 hyps2]
  (let [delta-fn (fn [hyps] (if (second hyps)
                              (- (hyp-conf workspace (first hyps))
                                 (hyp-conf workspace (second hyps)))
                              (hyp-conf workspace (first hyps))))
        hyps1-delta (delta-fn hyps1)
        hyps2-delta (delta-fn hyps2)]
    (if (= 0 (compare hyps1-delta hyps2-delta))
      (if (= 0 (compare (hyp-conf workspace (first hyps1))
                        (hyp-conf workspace (first hyps2))))
        (compare (:id (first hyps1)) (:id (first hyps2)))
        (compare (hyp-conf workspace (first hyps1))
                 (hyp-conf workspace (first hyps2))))
      (compare hyps1-delta hyps2-delta))))

(defn sort-explainers
  [workspace explainers]
  (let [internal-sorted (map (fn [expl]
                               (assoc expl :explainers
                                      (reverse (sort (partial compare-by-conf workspace)
                                                     (:explainers expl)))))
                             explainers)]
    (reverse (sort (fn [expl1 expl2]
                     (compare-by-delta workspace (:explainers expl1) (:explainers expl2)))
                   internal-sorted))))

(defn find-all-explainers
  [workspace]
  (let [g (:graph workspace)
        explainers (mapcat
                    (fn [h]
                      (let [hyps (filter #(not ((:accepted workspace) %))
                                         (incoming g h))
                            grouped (group-by :type hyps)]
                        (map (fn [expl] {:hyp h :explainers expl}) (vals grouped))))
                    (:unexplained workspace))]
    (sort-by (comp :id :hyp)
             (filter (comp first :explainers) explainers))))

(defn normalize-confidences
  "Normalize the apriori confidences of a collection of hyps.
   Returns a map with hyps as keys and new conf's as values."
  [hyps]
  (let [sum (reduce + 0.0 (map #(:apriori %) hyps))]
    (cond
     (= 1 (count hyps)) {(first hyps) 1.0}
     (= 0.0 sum) {}
     :else (reduce #(assoc %1 %2 (/ (:apriori %2) sum)) {} hyps))))

(defn update-confidences
  "Update confidences of hyps based on their normalized apriori
   confidences; if a normalized apriori confidence is better than the
   recorded confidence, update the recorded confidence. This function
   should only be called on a non-transitive (i.e. immediate)
   explanation seq."
  [workspace explainers]
  ;; for each seq of explainers
  (reduce (fn [ws {hyp :hyp alts :explainers}]
            (let [norm-alts (normalize-confidences alts)]
              ;; for each normalized confidence hyp
              (reduce (fn [ws2 hyp]
                        ;; average the normalized conf with existing conf
                        (assoc-in ws2 [:hyp-confidences hyp]
                                  (/ (+ (hyp-conf ws2 hyp) (get norm-alts hyp)) 2.0)))
                      ws (sort-by :id (keys norm-alts)))))
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
        ;; can't conflict with what it explains or what explains it
        hyps (set/difference (nodes g) (incoming g hyp) (neighbors g hyp))
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
  [workspace hyp]
  (let [expl (filter (nodes (get workspace :graph)) (:explains hyp))
        expl-static (filter (nodes (get workspace :graph-static)) (:explains hyp))
        g-added-static (reduce (fn [g n] (-> g (add-nodes n)
                                             (add-attr n :id (:id n))
                                             (add-attr n :label (:id n))))
                               (:graph-static workspace)
                               (conj expl-static hyp))
        g-edges-static (reduce (fn [g e] (add-edges g [hyp e]))
                               g-added-static expl-static)
        g-added (reduce (fn [g n] (-> g (add-nodes n)
                                      (add-attr n :id (:id n))
                                      (add-attr n :label (:id n))))
                        (:graph workspace) (conj expl hyp))
        g-edges (reduce (fn [g e] (add-edges g [hyp e])) g-added expl)]
    (-> workspace
        (assoc :graph-static g-edges-static)
        (assoc :graph g-edges)
        (assoc-in [:hyp-confidences hyp] (:apriori hyp))
        (update-in [:resources :hypothesis-count] inc)
        (update-in [:log :added] conj {:hyp hyp :explains expl}))))

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
  [workspace hyp alts]
  (let [commas (fn [ss] (apply str (interpose ", " (sort ss))))
        ws (-> workspace
               (update-in
                [:hyp-log hyp] conj
                (format "Accepted in cycle %d (alts: %s)"
                        (:cycle workspace) (commas (map :id alts))))
               (update-in [:accepted] conj hyp)
               (update-in [:unexplained] set/difference
                          (neighbors (:graph workspace) hyp))
               (update-in [:unexplained] conj hyp)
               (update-in [:graph-static] add-attr hyp :fontcolor "green")
               (update-in [:graph-static] add-attr hyp :color "green"))
        ws-alts (reduce (fn [ws2 alt] (update-in ws2 [:hyp-log alt] conj
                                                 (format "Alternate in cycle %d"
                                                         (:cycle workspace))))
                        ws alts)
        conflicts (find-conflicts ws-alts hyp)
        ws-rejected (reject-many ws-alts conflicts)]
    (update-in ws-rejected [:log :accrej (:cycle workspace)] conj
               {:acc hyp :rej conflicts})))

(defn add-fact
  [workspace hyp]
  (-> (add workspace hyp)
      (update-in [:unexplained] conj hyp)
      (update-in [:forced] conj hyp)
      (update-in [:log :forced] conj hyp)
      (update-in [:accepted] conj hyp)
      (update-in [:log :accepted] conj hyp)
      (update-in [:graph-static] add-attr hyp :fontcolor "gray50")
      (update-in [:graph-static] add-attr hyp :color "gray50")))

(defn reset-confidences
  [workspace]
  (let [hyps (get-hyps workspace)]
    (if (empty? hyps) workspace
        (assoc workspace :hyp-confidences
               (apply assoc {} (mapcat (fn [h] [h (:apriori h)]) hyps))))))

(defn measure-doubt
  [workspace]
  (let [acc-not-forced (set/difference (:accepted workspace) (:forced workspace))]
    (if (empty? acc-not-forced)
      (if (empty? (:unexplained (:final (:log workspace)))) 0.0 1.0)
      (let [confs (vals (select-keys (:hyp-confidences workspace) acc-not-forced))]
        (double (/ (reduce + 0.0 (map #(- 1.0 %) confs)) (count confs)))))))

(defn get-doubt
  [workspace]
  (:doubt (:log workspace)))

(defn get-unexp-pct
  "Only measure unexplained forced hyps."
  [workspace]
  (if (empty? (:forced workspace)) 0.0
      (double (/ (count (set/intersection (:forced workspace)
                                          (:unexplained (:final (:log workspace)))))
                 (count (:forced workspace))))))

(defn get-noexp-pct
  [workspace]
  (if (empty? (:forced workspace)) 0.0
      (double (/ (count (:no-explainers (:final (:log workspace))))
                 (count (:forced workspace))))))

(defn log-final
  [workspace explainers]
  (let [ws (assoc-in workspace [:log :final]
                     {:accepted (:accepted workspace)
                      :rejected (:rejected workspace)
                      :unexplained (:unexplained workspace)
                      :no-explainers (find-no-explainers workspace)
                      :unaccepted (set/difference
                                   (get-hyps workspace)
                                   (:accepted workspace)
                                   (:rejected workspace))
                      :last-explainers explainers})]
    (assoc-in ws [:log :doubt] (measure-doubt ws))))

(defn find-best
  [workspace explainers threshold]
  (if (empty? explainers) {}
      (let [essentials (filter #(= 1 (count (:explainers %))) explainers)]
        (if (not-empty essentials)
          ;; choose most-confident essential
          (let [expl (first essentials)
                best (first (:explainers expl))]
            {:best best
             :alts (disj (set (mapcat :explainers (rest essentials))) best)
             :essential? true :delta nil :explained (:hyp expl)})
          ;; otherwise, choose highest-delta non-essential
          (let [expl (first explainers)
                alts (:explainers expl)
                best (first alts)
                delta (- (hyp-conf workspace (first alts))
                         (hyp-conf workspace (second alts)))]
            (if (>= delta threshold)
              {:best best :alts (rest alts)
               :essential? false :delta delta :explained (:hyp expl)}))))))

(defn get-more-hyps
  [workspace]
  ;;TODO figure out order hyps should be attempted
  (first (filter not-empty
                 (map (fn [h] ((:hypothesize-fn (:abduction @problem)) h
                               (:accepted workspace) (:rejected workspace)))
                      (:unexplained workspace)))))

(defn explain
  [workspace]
  (loop [ws workspace]
    (if (empty? (:unexplained ws)) (log-final ws [])
        (let [explainers (find-all-explainers ws)]
          (if (empty? explainers)
            (if-let [hs (get-more-hyps ws)]
              (recur (reduce add ws hs))
              (log-final ws []))
            (let [ws-confs (update-confidences ws explainers)
                  explainers-sorted (sort-explainers ws-confs explainers)
                  {:keys [best alts essential? delta] :as b}
                  (find-best ws-confs explainers-sorted
                             (/ (:Threshold params) 100.0))]
              (if-not best
                (log-final ws-confs explainers-sorted)
                (recur
                 (let [ws-logged (-> ws-confs
                                     (update-in [:cycle] inc)
                                     (update-in [:resources :explain-cycles] inc)
                                     (update-in [:log :best] conj b))]
                   (accept ws-logged best alts))))))))))

(defn analyze
  [workspace hyp]
  (let [accepted? ((:accepted workspace) hyp)
        rejected? ((:rejected workspace) hyp)
        ;; hyps worth considering are those that this hyp explains (if
        ;; this hyp is accepted) or those that conflict with it (if
        ;; this hyp is rejected)
        hyps-consequent (cond
                         accepted? (neighbors (:graph-static workspace) hyp)
                         rejected? (set/intersection
                                    (:accepted workspace)
                                    (set (find-conflicts workspace hyp :static)))
                         :else #{})
        check-sets (set (mapcat (fn [n] (map set (combinations hyps-consequent n)))
                                [1 2 3 4]))]
    ;; attempt removing each forced hyp separately, and see if original
    ;; hyp is still accepted
    (loop [to-check check-sets
           acc []
           unacc []
           rej []]
      (if (empty? to-check) [acc unacc rej]
          (let [check (first to-check)
                ws (reset-confidences workspace)
                ws-altered (-> ws (update-in [:accepted] set/difference check)
                               (reject-many check))
                ws-explained (explain ws-altered)
                now-accepted? ((:accepted ws-explained) hyp)
                now-rejected? ((:rejected ws-explained) hyp)]
            (cond
             ;; hyp (newly) accepted
             (and rejected? now-accepted?)
             (recur (rest to-check) (conj acc check) unacc rej)
             ;; hyp (newly) rejected
             (and accepted? now-rejected?)
             (recur (rest to-check) acc unacc (conj rej check))
             ;; no change
             (or (and accepted? now-accepted?) (and rejected? now-rejected?))
             (recur (rest to-check) acc unacc rej)
             ;; else, turned into unaccepted
             :else
             (recur (rest to-check) acc (conj unacc check) rej)))))))

(defn add-sensor-hyps
  [workspace time time-now sensors]
  (let [msh (fn [s h t] ((:make-sensor-hyps-fn (:abduction @problem))
                         s h t time time-now))]
    (reduce (fn [ws t]
              (let [hs (mapcat (fn [s] (mapcat #(msh s % t) (sensed-at s t))) sensors)]
                (reduce add-fact ws hs)))
            workspace (range time (inc time-now)))))

(defn add-kb
  [ws]
  (reduce add-fact ws ((:generate-kb-fn (:abduction @problem)))))

(defn init-workspace
  []
  (add-kb
   {:graph (digraph)
    :graph-static (digraph)
    :cycle 0
    :hyp-confidences {}
    :log {:added [] :forced [] :best [] :accrej {}
          :final {:accepted [] :rejected []
                  :unexplained [] :no-explainers [] :unaccepted []}
          :doubt nil}
    :hyp-log {}
    :conf nil
    :unexplained #{}
    :accepted #{}
    :forced #{}
    :rejected #{}
    :resources {:explain-cycles 0 :hypothesis-count 0}}))
