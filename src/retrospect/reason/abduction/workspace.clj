(ns retrospect.reason.abduction.workspace
  (:import (misc AlphanumComparator))
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:use [loom.io :only [dot-str]])
  (:use [loom.graph :only
         [digraph nodes incoming neighbors weight
          add-nodes add-edges remove-nodes edges transpose has-edge?]])
  (:use [loom.alg :only [pre-traverse]])
  (:use [loom.attr :only [add-attr remove-attr]])
  (:use [clojure.contrib.combinatorics :only [combinations]])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.profile :only [prof]])
  (:use [retrospect.logging])
  (:use [retrospect.random])
  (:use [retrospect.state]))

(defrecord Hypothesis
    [id type subtype needs-explainer?
     conflict apriori explains boosts short-str desc]
  Object
  (toString [self] (format "%s(%s)" id short-str))
  Comparable
  (compareTo [self other] (compare (hash self) (hash other))))

(defn explains
  [hyp]
  (if (:TransitiveExplanation params)
    (filter #(not= hyp %) (tree-seq #(not-empty (:explains %)) :explains hyp))
    (:explains hyp)))

(defmethod print-method Hypothesis
  [o w]
  (print-simple (format "%s(%s)" (:id o) (:short-str o)) w))

(def empty-workspace
  {:graph (digraph)
   :oracle nil
   :cycle 0
   ;; remember the order hyps were added
   :added []
   :initial-kb []
   :log {:unexplained [] :best [] :accrej {}}
   :hyp-log {}
   ;; a list of hyps that explain each key;
   ;; hyps that are keys need to be explained
   :active-explainers {}
   ;; all explainers; serves as a cache
   :explainers {}
   ;; a map of type => seq
   :hypotheses {}
   ;; hypotheses that are neither accepted nor rejected
   :available #{}
   :hyp-confidences {}
   :forced #{}
   ;; a map of type => set
   :accepted {}
   ;; a map of type => set
   :rejected {}})

(defn new-hyp
  [prefix type subtype needs-explainer?
   conflict apriori explains boosts short-str desc data]
  (let [id (inc last-id)]
    (set-last-id id)
    ;; use var-set if running batch mode; def if using player or repl
    ;; (in batch mode, thread is called something like pool-2-thread-1)

    (merge
     (Hypothesis. (format "%s%d" prefix id)
                  type subtype needs-explainer? conflict
                  apriori explains boosts short-str desc)
     data)))

(defn accepted?
  [workspace hyp]
  ((get-in workspace [:accepted (:type hyp)] #{}) hyp))

(defn rejected?
  [workspace hyp]
  ((get-in workspace [:rejected (:type hyp)] #{}) hyp))

(defn hyp-log
  [workspace hyp]
  (get (:hyp-log workspace) hyp))

(defn hyp-conf
  [workspace hyp]
  (if (:UseScores params)
    (get (:hyp-confidences workspace) hyp)
    1.0))

(defn find-no-explainers
  [workspace]
  (set (filter (fn [h] (empty? (filter #(not= :more (:type %))
                                       (get (:explainers workspace) h))))
               (filter :needs-explainer? (apply concat (vals (:accepted workspace)))))))

(defn compare-by-conf
  "Since we are using probabilities, smaller value = less
   confidence. We want most confident first. With equal confidences,
   we look for higher explanatory power (explains more). If all that
   fails, comparison is done by the :id's (to keep it deterministic)."
  [workspace hyp1 hyp2]
  (prof :compar-by-conf-expl
        (let [conf-diff (double (- (hyp-conf workspace hyp1)
                                   (hyp-conf workspace hyp2)))
              expl (- (compare (count (explains hyp1))
                               (count (explains hyp2))))
              explainers (- (compare (count (get-in workspace [:explainers hyp1] []))
                                     (count (get-in workspace [:explainers hyp2] []))))
              id (compare (:id hyp1) (:id hyp2))]
          (if (<= (Math/abs conf-diff) (/ (:ConfThreshold params) 100))
            (if (= 0 expl)
              (if (= 0 explainers) id explainers)
              expl)
            (- (compare conf-diff 0))))))

(defn compare-by-delta
  [workspace hyps1 hyps2]
  (prof :compare-by-delta
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
              (- (compare (hyp-conf workspace (first hyps1))
                          (hyp-conf workspace (first hyps2)))))
            (- (compare hyps1-delta hyps2-delta))))))

(defn sort-explainers
  [workspace explainers]
  (prof :sort-explainers
        (let [hyp-sorter (cond (= (:HypPreference params) "abd")
                               #(sort (partial compare-by-conf workspace) %)
                               (= (:HypPreference params) "arbitrary")
                               #(my-shuffle %)
                               :else
                               #(sort (partial compare-by-conf workspace) %))
              expl-sorter (cond (= (:ContrastPreference params) "delta")
                                (fn [hs] (sort #(compare-by-delta
                                                 workspace (:expl %1) (:expl %2)) hs))
                                (= (:ContrastPreference params) "arbitrary")
                                #(my-shuffle %)
                                :else
                                (fn [hs] (sort #(compare-by-delta
                                                 workspace (:expl %1) (:expl %2)) hs)))]
          (expl-sorter (map #(update-in % [:expl] hyp-sorter) explainers)))))

(defn find-all-explainers
  [workspace]
  (let [expl-filter (if (:RequireExplainedAccepted params)
                      (fn [e]
                        (and ((:available workspace) e)
                             (every? #(accepted? workspace %) (explains e))))
                      (fn [e] ((:available workspace) e)))]
    (prof :find-all-explainers
          (sort-by (comp :id first)
                   (filter (comp first :expl)
                           (map
                            (fn [h] {:hyp h
                                     :expl (filter expl-filter
                                                   (get (:active-explainers workspace) h))})
                            (keys (:active-explainers workspace))))))))

(defn normalize-confidences
  "Normalize the apriori confidences of a collection of hyps.
   Returns a map with hyps as keys and new conf's as values."
  [hyp-map]
  (prof :normalize-confidences
        (let [sum (reduce + 0.0 (vals hyp-map))]
          (cond
           (= 1 (count hyp-map)) {(first (keys hyp-map)) 1.0}
           (> 0.0001 sum) (reduce #(assoc %1 %2 (/ 1.0 (count hyp-map)))
                                  hyp-map (keys hyp-map))
           :else (reduce #(update-in %1 [%2] / sum) hyp-map (keys hyp-map))))))

(defn normalize-confidences-groups
  [hyps workspace]
  (let [gs (reduce (fn [m h] (assoc m h (hyp-conf workspace h))) {} hyps)]
    (normalize-confidences gs)))

(defn update-confidences
  "Update confidences of hyps based on their normalized apriori
   confidences; if a normalized confidence is better than the recorded
   confidence, update the recorded confidence. This function should
   only be called on a non-transitive (i.e. immediate) explanation
   seq."
  [workspace explainers]
  (prof :update-confidences
        ;; for each seq of explainers
        (reduce (fn [ws {hyp :hyp alts :expl}]
                  (let [norm-alts (normalize-confidences-groups alts ws)]
                    ;; for each normalized confidence hyp
                    (reduce (fn [ws2 hyp]
                              ;; take the min normalized conf with existing conf
                              (assoc-in ws2 [:hyp-confidences hyp]
                                        (cond (= "max" (:ConfAdjustment params))
                                              (max (hyp-conf ws2 hyp) (get norm-alts hyp))
                                              (= "min" (:ConfAdjustment params))
                                              (min (hyp-conf ws2 hyp) (get norm-alts hyp))
                                              (= "avg" (:ConfAdjustment params))
                                              (/ (+ (hyp-conf ws2 hyp) (get norm-alts hyp)) 2.0)
                                              (= "none" (:ConfAdjustment params))
                                              (hyp-conf ws2 hyp)
                                              (= "norm" (:ConfAdjustment params))
                                              (get norm-alts hyp)
                                              :else
                                              (max (hyp-conf ws2 hyp) (get norm-alts hyp)))))
                            ws (sort-by :id (keys norm-alts)))))
                workspace explainers)))

(defn boost
  [workspace hyp]
  (if (or (not (:ApplyBoosting params))
          (not ((:available workspace) hyp)))
    workspace
    (let [new-conf (min 1.0 (+ 0.25 (hyp-conf workspace hyp)))]
      (-> workspace
          (assoc-in [:hyp-confidences hyp] new-conf)
          (update-in [:hyp-log hyp] conj
                     (format "Confidence boosted in cycle %d from %.2f to %.2f"
                             (:cycle workspace) (hyp-conf workspace hyp) new-conf))))))

(defn find-conflicts-selected
  "If a hypothesis's :conflict value is a function (a predicate), that function
   is called with the hyp and each other hyp. If the :conflict value is
   a keyword, the conflicts of a hyp are those other hyps that share the
   :conflict keyword"
  [workspace hyp hyps]
  (prof :find-conflicts-selected
        (let [c (:conflict hyp)]
          (cond
           ;; no conflict id; so it conflicts with nothing
           (nil? c) []

           ;; we have a function (predicate), so call the function on other hyps
           (fn? c) (filter #(and (not= % hyp) (c hyp %)) hyps)

           ;; otherwise, hyps conflict if their conflict ids are identical
           :else
           (filter #(and (not= % hyp) (= c (:conflict %))) hyps)))))

(defn find-conflicts
  [workspace hyp]
  (find-conflicts-selected workspace hyp (apply concat (vals (:hypotheses workspace)))))

(defn add-explainers
  [workspace hyp]
  (prof :add-explainers
        (reduce (fn [ws h]
                  (let [ws2 (update-in ws [:explainers h] conj hyp)]
                    ;; don't add active-explainers to hyps that have already
                    ;; been fully explained (indicated by not present in
                    ;; :active-explainers)
                    (if-not (get (:active-explainers ws2) h) ws2
                            (update-in ws2 [:active-explainers h] conj hyp))))
                workspace (explains hyp))))

(defn remove-explainers
  [workspace hyp]
  (prof :remove-explainers
        (do
          (log "Removing explainers for" hyp)
          (reduce (fn [ws h] (if (nil? (get (:active-explainers ws) h)) ws
                                 (update-in ws [:active-explainers h] disj hyp)))
                  workspace (explains hyp)))))

(defn reject-many
  [workspace hyps]
  (prof :reject-many
        (let [rejectable
              (prof :reject-many-rejectable
                    (doall (filter
                            (fn [h] (and (not (accepted? workspace h))
                                         (not (rejected? workspace h))))
                            hyps)))]
          (when (not-empty rejectable)
            (log "Rejecting" (str/join ", " rejectable)))
          (let [ws (prof :reject-many-update
                         (reduce
                          (fn [ws hyp]
                            (let [rej-prior (get-in ws [:rejected (:type hyp)] #{})]
                              (-> ws
                                  (remove-explainers hyp)
                                  (update-in [:active-explainers] dissoc hyp)
                                  (assoc-in [:rejected (:type hyp)] (conj rej-prior hyp))
                                  (update-in [:available] disj hyp)
                                  (update-in [:hyp-log hyp] conj
                                             (format "Rejected in cycle %d"
                                                     (:cycle workspace))))))
                          workspace rejectable))]
            (if @batch ws
                (prof :reject-many-graph
                      (assoc ws :graph
                             (reduce (fn [g r] (-> g (add-attr r :fontcolor "red")
                                                   (add-attr r :color "red")))
                                     (:graph ws) rejectable))))))))

(defn add
  [workspace hyp]
  (prof :add
        (if (prof :add-dup-search
                  (some (partial (:hyps-equal?-fn (:abduction @problem)) hyp)
                        (get (:hypotheses workspace) (:type hyp))))
          workspace
          (let [ws-needs-explainer
                (prof :add-needs-explainer
                      (if-not ((:active-explainers workspace) hyp) workspace
                              (assoc-in workspace [:active-explainers hyp] #{})))
                ws-explainers
                (prof :add-ws-update
                      (-> ws-needs-explainer
                          (update-in [:added] conj hyp)
                          (update-in [:available] conj hyp)
                          (add-explainers hyp)
                          (assoc-in
                           [:hyp-confidences hyp]
                           (cond (= :kb (:type hyp)) 1.0
                                 (some #(= (:type hyp) %)
                                       (map keyword (str/split (:Oracle params) #",")))
                                 (if ((:oracle workspace) hyp) 1.0 0.0)
                                 :else
                                 (:apriori hyp)))
                          (update-in [:hypotheses (:type hyp)] conj hyp)))
                conflicts
                (prof :add-conflicts
                      (find-conflicts-selected
                       ws-explainers hyp
                       (apply concat (vals (:accepted ws-explainers)))))
                ws-final
                (if @batch ws-explainers
                    (prof :add-graph-update
                          (let [g-added (reduce (fn [g n] (-> g (add-nodes n)
                                                              (add-attr n :id (:id n))
                                                              (add-attr n :label (:id n))))
                                                (:graph ws-explainers)
                                                (conj (explains hyp) hyp))
                                g-expl (reduce (fn [g e] (add-edges g [hyp e]))
                                               g-added (explains hyp))]
                            (assoc ws-explainers :graph g-expl))))]
            (prof :add-final
                  (if (empty? conflicts) ws-final
                      (reject-many ws-final [hyp])))))))

(defn update-graph
  [workspace]
  (if @batch workspace
      (prof :update-graph
            (assoc workspace :graph
                   (reduce
                    (fn [g h]
                      (let [conflicts (find-conflicts workspace h)]
                        (reduce (fn [g2 c]
                                  (if (or (has-edge? g2 h c) (has-edge? g2 c h)) g2
                                      (-> g2 (add-edges [h c])
                                          (add-attr h c :dir "none")
                                          (add-attr h c :style "dotted")
                                          (add-attr h c :constraint false))))
                                g conflicts)))
                    (:graph workspace) (apply concat (vals (:hypotheses workspace))))))))

(defn accept
  [workspace hyp explained delta essential?]
  (prof :accept
        (if (prof :accept-dup-check (or (accepted? workspace hyp)
                                        (rejected? workspace hyp)))
          workspace
          (do (log "Accepting" hyp)
              (let [ws-needs-exp
                    (prof :accept-needs-exp
                          (if-not ((:explainers workspace) hyp) workspace
                                  (assoc-in workspace [:active-explainers hyp]
                                            (set ((:explainers workspace) hyp)))))
                    acc-prior (get-in workspace [:accepted (:type hyp)] #{})
                    ws-acc
                    (prof :accept-update
                          (-> ws-needs-exp
                              (update-in [:hyp-log hyp] conj
                                         (format (str "Accepted in cycle %d "
                                                      "to explain %s with delta %.2f"
                                                      " (essential? %s)")
                                                 (:cycle workspace)
                                                 explained delta essential?))
                              (assoc-in [:accepted (:type hyp)] (conj acc-prior hyp))
                              (update-in [:available] disj hyp)
                              (update-in [:graph] add-attr hyp :fontcolor "green")
                              (update-in [:graph] add-attr hyp :color "green")))
                    ws-expl (reduce (fn [ws2 h]
                                      (update-in ws2 [:active-explainers] dissoc h))
                                    ws-acc (explains hyp))
                    conflicts (find-conflicts ws-expl hyp)
                    ws-conflicts (reject-many ws-expl conflicts)
                    ws-boosts (reduce boost ws-conflicts (:boosts hyp))]
                (update-in ws-boosts [:log :accrej (:cycle ws-boosts)] conj
                           {:acc hyp :rej conflicts}))))))

(defn add-fact
  [workspace hyp]
  (prof :add-fact
        (if (prof :add-fact-dup-check
                  (some (partial (:hyps-equal?-fn (:abduction @problem)) hyp)
                        (get (:hypotheses workspace) (:type hyp))))
          workspace
          (let [acc-prior (get-in workspace [:accepted (:type hyp)] #{})]
            (-> (add workspace hyp)
                (update-in [:forced] conj hyp)
                (assoc-in [:accepted (:type hyp)] (conj acc-prior hyp))
                (assoc-in [:active-explainers hyp] #{})
                (update-in [:graph] add-attr hyp :fontcolor "gray50")
                (update-in [:graph] add-attr hyp :color "gray50"))))))

(defn get-unexp-pct
  "Only measure unexplained \"needs-explainer\" hyps."
  [workspace]
  (if (empty? (filter :needs-explainer? (apply concat (vals (:accepted workspace))))) 0.0
      (/ (double (count (:unexplained (:log workspace))))
         (double (count (filter :needs-explainer?
                                (apply concat (vals (:accepted workspace)))))))))

(defn get-noexp-pct
  [workspace]
  (if (empty? (filter :needs-explainer? (apply concat (vals (:accepted workspace))))) 0.0
      (/ (double (count (find-no-explainers workspace)))
         (double (count (filter :needs-explainer?
                                (apply concat (vals (:accepted workspace)))))))))

(defn calc-doubt
  [workspace]
  (let [acc-not-forced (filter #(not ((:forced workspace) %))
                               (apply concat (vals (:accepted workspace))))]
    (if (empty? acc-not-forced)
      (if (empty? (:unexplained (:log workspace))) 0.0 1.0)
      (let [confs (vals (select-keys (:hyp-confidences workspace) acc-not-forced))]
        (reduce + 0.0 (map #(- 1.0 %) confs))))))

(defn calc-coverage
  [workspace]
  (let [accessible (set
                    (mapcat (fn [h] (pre-traverse (:graph workspace) h))
                            (set/difference
                             (set (apply concat (vals (:accepted workspace))))
                             (:forced workspace))))]
    (/ (double (count (set/intersection (:forced workspace) accessible)))
       (double (count (:forced workspace))))))

(defn find-unaccepted
  [workspace]
  (set/difference
   (set (apply concat (vals (:hypotheses workspace))))
   (set (apply concat (vals (:accepted workspace))))
   (set (apply concat (vals (:rejected workspace))))))

(defn log-final
  [workspace explainers]
  (prof :log-final
        (-> workspace
            (update-in [:log] merge
                       {:unexplained (keys (:active-explainers workspace))
                        :last-explainers explainers})
            (dissoc :prior-explainers))))

(defn find-best
  [workspace explainers threshold]
  (prof :find-best
        (if (empty? explainers) {}
            (let [essential (first (filter #(nil? (second (:expl %))) explainers))]
              (if essential
                (let [best (first (:expl essential))]
                  (log "Choosing best (essential)" best)
                  {:best best :essential? true :delta nil :explained (:hyp essential)})
                ;; otherwise, choose highest-delta non-essential
                (let [expl (first explainers)
                      best (first (:expl expl))
                      next-best (second (:expl expl))
                      delta (- (hyp-conf workspace best)
                               (hyp-conf workspace next-best))]            
                  (when (>= delta threshold)
                    (log "Choosing best" best "delta" delta)
                    {:best best :essential? false :delta delta
                     :explained (:hyp expl)})))))))

(defn remove-hyp
  [workspace hyp]
  (log "Removing" (:id hyp))
  (-> workspace
      (remove-explainers hyp)
      (update-in [:graph] remove-nodes hyp)
      (update-in [:hyp-confidences] dissoc hyp)
      (update-in [:hypotheses (:type hyp)] (fn [hs] (filter #(not= % hyp) hs)))
      (update-in [:active-explainers] dissoc hyp)
      (update-in [:available] disj hyp)
      (update-in [:explainers] dissoc hyp)))

(defn update-kb
  [workspace]
  (if-not (:UpdateKB params) workspace
          (let [old-kb-hyps (get (:hypotheses workspace) :kb)
                new-kb-hyps ((:update-kb-fn (:abduction @problem))
                             (:accepted workspace)
                             (:unexplained (:log workspace))
                             (:hypotheses workspace))]
            (-> workspace (assoc :initial-kb new-kb-hyps)
                (assoc-in [:accepted :kb] new-kb-hyps)))))

(defn update-hypotheses
  [workspace]
  (let [hyps ((:hypothesize-fn (:abduction @problem))
              (:forced workspace) (:accepted workspace) (:hypotheses workspace))]
    (reduce add workspace hyps)))

(defn explain
  [workspace]
  (prof :explain
        (loop [ws workspace]
          (log "Explaining again...")
          (let [explainers (if (and (= "none" (:ConfAdjustment params))
                                    (:prior-explainers ws))
                             (prof :explain-prior-filter
                                   (filter (comp first :expl)
                                           (map (fn [es]
                                                  (assoc es :expl
                                                         (filter #((:available ws) %)
                                                                 (:expl es))))
                                                (filter #((:active-explainers ws) (:hyp %))
                                                        (:prior-explainers ws)))))
                             (find-all-explainers ws))]
            (if (empty? explainers)
              (do (log "No explainers. Done.")
                  (update-kb (log-final ws [])))
              (let [ws-confs (if (= "none" (:ConfAdjustment params)) ws
                                 (update-confidences ws explainers))
                    explainers-sorted (if (and (= "none" (:ConfAdjustment params))
                                               (:prior-explainers ws-confs))
                                        explainers
                                        (sort-explainers ws-confs explainers))
                    {:keys [best essential? explained delta] :as b}
                    (find-best ws-confs explainers-sorted
                               (/ (:Threshold params) 100.0))]
                (if-not best
                  (do (log "No best. Done.")
                      (update-kb (log-final ws-confs explainers-sorted)))
                  (do (log "Best is" (:id best) (hyp-conf ws-confs best))
                      (let [ws-accepted
                            (let [ws-logged (-> ws-confs
                                                (update-in [:cycle] inc)
                                                (update-in [:log :best] conj b))]
                              (accept ws-logged best explained delta essential?))]
                        (recur (assoc ws-accepted :prior-explainers explainers-sorted)))))))))))

(defn revise
  [workspace hyp]
  ;; don't 'revise' if already accepted this hyp
  (if (some #{hyp} (get-in workspace [:accepted (:type hyp)])) workspace
      ;; here is "belief revision" in all its glory
      (let [conflicts (find-conflicts-selected
                       workspace hyp (apply concat (vals (:accepted workspace))))]
        #_(println "revising with" hyp "conflicts" conflicts)
        (-> (reduce (fn [ws h] (update-in ws [:accepted (:type h)] disj h))
                    workspace conflicts)
            (add hyp)
            (accept hyp [] nil false)
            (assoc :prior-explainers nil)
            (explain)))))

(defn add-sensor-hyps
  [workspace time-prev time-now sensors]
  (let [hs (mapcat (fn [s] ((:make-sensor-hyps-fn (:abduction @problem))
                            s time-prev time-now (:hypotheses workspace)))
                   sensors)]
    (reduce add-fact workspace hs)))

(defn add-kb
  [workspace hyps]
  (reduce (fn [ws h]
            (let [acc-prior (get-in ws [:accepted (:type h)] #{})]
              (-> ws (add h)
                  (assoc-in [:accepted (:type h)] (conj acc-prior h))
                  (update-in [:initial-kb] conj h))))
          workspace hyps))

(defn init-kb
  [workspace training]
  (add-kb workspace ((:generate-kb-fn (:abduction @problem)) training)))

(defn reset-workspace
  [workspace]
  (reduce (fn [ws h] (if ((:forced workspace) h) (add-fact ws h) (add ws h)))
          (add-kb (assoc empty-workspace :oracle (:oracle workspace))
                  (:initial-kb workspace))
          (:forced workspace)))

(defn init-workspace
  ([] (when-let [f (:reset-fn (:abduction @problem))] (f))
     empty-workspace)
  ([workspace]
     (when-let [f (:reset-fn (:abduction @problem))] (f))
     (add-kb empty-workspace (:initial-kb workspace))))
