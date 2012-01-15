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
  (:use [clojure.contrib.combinatorics :only [combinations]])
  (:use [retrospect.state]))

(def last-id 0)

(def meta? false)

(defn set-last-id
  [n]
  (def last-id n))

(defrecord Hypothesis
    [id type subtype conflict apriori expl-func explains depends desc data]
  Object
  (toString [self] (format "%s: %s" id desc))
  Comparable
  (compareTo [self other] (compare (hash self) (hash other))))

(defmethod print-method Hypothesis
  [o w]
  (print-simple (:id o) w))

(comment   (print-simple (str "#<" (:id o) ": \"" (:desc o) "\">") w))

(defn new-hyp
  [prefix type subtype conflict apriori expl-func explains depends desc data]
  (let [id (inc last-id)]
    ;; use var-set if running batch mode; def if using player or repl
    ;; (in batch mode, thread is called something like pool-2-thread-1)
    (if (or (= "AWT-EventQueue-0" (. (Thread/currentThread) getName))
            (= "Thread-1" (. (Thread/currentThread) getName))) 
      (def last-id (inc last-id))
      (var-set (var last-id) (inc last-id)))
    (Hypothesis. (format "%s%d" (if meta? (str "M" prefix) prefix) id)
                 type subtype conflict apriori expl-func explains depends desc data)))

(defn init-workspace
  ([]
     {:graph nil
      :graph-static (digraph)
      :cycle 0
      :hyp-confidences {}
      :log {:added [] :forced [] :best [] :accrej {}
            :final {:accepted [] :rejected []
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

(defn transitive-explainer-paths
  [workspace hyp]
  (let [g (:graph-static workspace)
        paths (loop [ps [[hyp]]]
                (let [newps (set (concat ps (mapcat (fn [p] (let [es (incoming g (first p))]
                                                              (map (fn [e] (concat [e] p)) es)))
                                                    ps)))]
                  (if (= (count newps) (count ps))
                    (filter not-empty (map butlast ps))
                    (recur newps))))]
    ;; only want "paths" that consist of hyps "between" the start and end
    (reduce (fn [m path] (update-in m [(first path)] conj (rest path)))
            {} (filter (comp not-empty rest) paths))))

(defn cache-transitive-explainers
  [workspace]
  (let [tg (transpose (:graph-static workspace))
        trans-expls
        (reduce (fn [m hyp]
                  (assoc m hyp {:explainers
                                (set (mapcat #(pre-traverse tg %)
                                             (neighbors tg hyp)))
                                :paths (transitive-explainer-paths workspace hyp)}))
                {} (nodes (:graph-static workspace)))]
    (assoc workspace :trans-expls trans-expls)))

(defn transitive-explainer-blocked?
  "This is needed due to the trans-expl cache. When a intermediate hyp
   is rejected, there is no longer a path in from the transitive hyp
   through the intermediate; but since the trans-expl cache is not
   continually updated, we need to check if a transitive path is still
   valid."
  [workspace hyp explainer & opts]
  (let [g (if (some #{:static} opts)
            (:graph-static workspace)
            (:graph workspace))
        trans-expls (if (nil? (:trans-expls workspace))
                      (cache-transitive-explainers workspace)
                      (:trans-expls workspace))]
    (and
     (not-empty (get (:paths (get trans-expls hyp)) explainer))
     (every? (fn [path] (some (fn [h] ((:rejected workspace) h)) path))
             (get (:paths (get trans-expls hyp)) explainer)))))

(defn incoming-transitive
  "Find transitive explainers of some hypothesis. Presumably this
   function is only used when transitive-explanation is active. It
   relies on a cache built by (cache-transitive-explainers). Only
   valid transitive explainers are returned; a transitive explainer is
   valid if it is not \"blocked\", where being blocked means all paths
   from the hyp to the explainer go through one or more rejected
   nodes."
  [workspace hyp & opts]
  (let [g (if (some #{:static} opts)
            (:graph-static workspace)
            (:graph workspace))
        trans-expls (if (nil? (:trans-expls workspace))
                      (cache-transitive-explainers workspace)
                      (:trans-expls workspace))]
    (set (filter #(not (apply transitive-explainer-blocked? workspace hyp % opts))
                 (set/intersection (:explainers (get trans-expls hyp)) (nodes g))))))

(defn find-explainers
  [workspace hyp & opts]
  (let [g (if (some #{:static} opts)
            (:graph-static workspace)
            (:graph workspace))]
    (vals
     (group-by :type
               (if (:TransitiveExplanation params)
                 (sort-by :id (set/union (incoming g hyp)
                                         (apply incoming-transitive workspace hyp opts)))
                 (sort-by :id (incoming g hyp)))))))

(defn find-unexplained
  [workspace]
  (set (filter #(and (or ((:forced workspace) %)
                         (not-empty (incoming (:graph-static workspace) %)))
                     ((nodes (:graph workspace)) %))
               (:accepted workspace))))

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
  [workspace trans?]
  (let [g (:graph workspace)
        and-expl #(every? (:accepted workspace) (neighbors g %))
        or-expl #(some (:accepted workspace) (neighbors g %))
        filter-func (fn [h] (cond (= :or (:expl-func h)) or-expl
                                  (= :and (:expl-func h)) and-expl
                                  :else (constantly true)))
        explainers (mapcat
                    (fn [h]
                      (let [hyps (filter #(not ((:accepted workspace) %))
                                         (if trans? (incoming-transitive workspace h)
                                             (incoming g h)))
                            grouped (group-by :type hyps)]
                        (map (fn [expl] {:hyp h :explainers expl}) (vals grouped))))
                    (find-unexplained workspace))]
    (sort-by (comp :id :hyp)
             (filter (comp first :explainers)
                     (if trans? explainers
                         (map (fn [expl]
                                (assoc expl :explainers
                                       (filter (fn [h] ((filter-func h) h))
                                               (:explainers expl))))
                              explainers))))))

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
                             (update-in [:resources :hypothesis-count] inc)
                             (update-in [:log :added] conj {:hyp hyp :explains expl}))]
          ;; abort if added hyp conflicts with something accepted (only when adding
          ;; hyps after explaining has begun)
          (if (and (not-any? #{:static} opts)
                   (some (:accepted ws-updated) (find-conflicts ws-updated hyp)))
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
  [workspace hyp alts pdata]
  (let [g (:graph workspace)
        ;; removable hyps are those that are explained (by the newly
        ;; accepted hyp) and that explain nothing (that hasn't already
        ;; been removed)
        removable (concat (filter (fn [h] (empty? (neighbors g h)))
                                  (neighbors g hyp))
                          (if (empty? (incoming g hyp)) [hyp] []))
        ;; must find conflicts in original workspace, before explained
        ;; hyps are removed
        conflicts (find-conflicts workspace hyp)
        new-g (apply remove-nodes g removable)
        commas (fn [ss] (apply str (interpose ", " (sort ss))))
        ws (-> workspace
               (update-in
                [:hyp-log hyp] conj
                (format "Accepted in cycle %d (removed: %s, alts: %s)"
                        (:cycle workspace)
                        (commas (map :id (filter #(not ((:forced workspace) %)) removable)))
                        (commas (map :id alts))))
               (update-in [:accepted] conj hyp)
               (update-in [:graph-static] add-attr hyp :fontcolor "green")
               (update-in [:graph-static] add-attr hyp :color "green")
               (assoc :graph new-g))
        ws-alts (reduce (fn [ws2 alt] (update-in ws2 [:hyp-log alt] conj
                                                 (format "Alternate in cycle %d"
                                                         (:cycle workspace))))
                        ws alts)]
    (loop [ws (reject-many ws-alts conflicts)
           rejected #{}]
      (let [hyps (sort-by :id (set/union (:accepted ws)
                                         (apply concat (find-all-explainers ws false))))
            incon (set/difference (set ((:inconsistent-fn @problem)
                                        pdata hyps (:rejected ws)))
                                  rejected)]
        (if (not-empty incon)
          (recur (reject-many ws incon) (set/union rejected (set incon)))
          (update-in ws [:log :accrej (:cycle workspace)] conj
                     {:acc hyp :rej (concat conflicts rejected)}))))))

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

(defn transitive-accept
  "Need to accept the transitive explainer last so that any lower,
   accepted hypotheses remove the appropriate hypotheses that they
   explain. A hypothesis is only accepted via transitive explanation
   if that hypothesis is a member of every path from the explained hyp
   and the accepted hyp."
  [workspace hyp alts pdata]
  (let [paths (find-explains-transitive-paths workspace hyp)
        acceptable (set/difference
                    (set (concat
                          ;; add what the hyp explains (as "acceptable")
                          ;; if the hyp required that each of its explained
                          ;; parts are acceptable (i.e. not blocked)
                          (if (= :and (:expl-func hyp))
                            (:explains hyp) [])
                          (cond (empty? paths) []
                                (= 1 (count paths)) (first paths)
                                :else (apply set/intersection (map set paths)))))
                    (:accepted workspace))
        ws (reduce (fn [ws2 h] (transitive-accept ws2 h [] pdata))
                   workspace (sort-by :id acceptable))]
    (accept ws hyp alts pdata)))

(defn force-accept
  [workspace hyp]
  (-> workspace
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

(defn prepare-workspace
  "Clear the decision, except for what was 'forced'."
  [workspace]
  (let [ws (assoc workspace :doubt nil :accepted #{} :rejected #{}
                  :graph (:graph-static workspace))]
    (-> ws
        (update-in [:graph-static]
                   #(reduce (fn [g h] (-> g (remove-attr h :fontcolor)
                                          (remove-attr h :color)))
                            % (set/difference (nodes %) (:forced ws))))
        (assoc-in [:log :accepted] (:forced workspace))
        (assoc-in [:accepted] (:forced workspace)))))

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

(defn measure-unexplained-pct
  "Only measure unexplained forced hyps."
  [workspace]
  (if (empty? (:forced workspace)) 0.0
      (double (/ (count (set/intersection (:forced workspace)
                                          (:unexplained (:final (:log workspace)))))
                 (count (:forced workspace))))))

(defn get-unexplained-pct
  [workspace]
  (:unexplained-pct (:log workspace)))

(defn log-final
  [workspace immediate-explainers transitive-explainers]
  (let [ws (assoc-in workspace [:log :final]
                     {:accepted (:accepted workspace)
                      :rejected (:rejected workspace)
                      :unexplained (find-unexplained workspace)
                      :no-explainers (find-no-explainers workspace)
                      :unaccepted (set/difference
                                   (get-hyps workspace)
                                   (:accepted workspace)
                                   (:rejected workspace))
                      :last-immediate-explainers immediate-explainers
                      :last-transitive-explainers
                      (if (:TransitiveExplanation params) transitive-explainers [])})
        ws2 (assoc-in ws [:log :unexplained-pct] (measure-unexplained-pct ws))]
    (assoc-in ws2 [:log :doubt] (measure-doubt ws2))))

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
  (let [trans? (:TransitiveExplanation params)
        ws-trans (if trans? (cache-transitive-explainers workspace)
                     workspace)]
    (loop [ws ws-trans]
      (if (empty? (edges (:graph ws))) (log-final ws [] [])
          (let [im-expl (find-all-explainers ws false)
                tr-expl (if trans? (find-all-explainers ws true) [])]
            (if (or (and (empty? im-expl) (not trans?))
                    (and (empty? im-expl) (empty? tr-expl)))
              (log-final ws (sort-explainers ws im-expl) (sort-explainers ws tr-expl))
              (let [ws-confs (update-confidences ws im-expl)
                    ws-confs2 (if trans? (update-confidences ws-confs tr-expl) ws-confs)
                    im-expl-sorted (sort-explainers ws-confs2 im-expl)
                    tr-expl-sorted (sort-explainers ws-confs2 tr-expl)
                    {:keys [best alts essential? transitive? delta] :as b}
                    (find-best-multi ws-confs2 im-expl-sorted tr-expl-sorted
                                     (/ (:Threshold params) 100.0))]
                (if-not best
                  (log-final ws-confs2 im-expl-sorted tr-expl-sorted)
                  (recur
                   (let [ws-logged (-> ws-confs2
                                       (update-in [:cycle] inc)
                                       (update-in [:resources :explain-cycles] inc)
                                       (update-in [:log :best] conj b))]
                     (if transitive? ;; if best was transitive explainer
                       (transitive-accept ws-logged best alts pdata)
                       (accept ws-logged best alts pdata))))))))))))

(defn analyze
  [workspace hyp pdata]
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
                ws (prepare-workspace workspace)
                ws-altered (-> ws (update-in [:accepted] set/difference check)
                               (reject-many check))
                ws-explained (explain ws-altered pdata)
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
