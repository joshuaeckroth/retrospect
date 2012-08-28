(ns retrospect.reason.abduction.workspace
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:use [clojure.contrib.core :only [dissoc-in]])
  (:use [loom.graph :only
         [digraph nodes incoming neighbors weight
          add-nodes add-edges remove-nodes edges has-edge?]])
  (:use [loom.alg :only [pre-traverse]])
  (:use [loom.attr :only [add-attr remove-attr]])
  (:use [retrospect.profile :only [prof]])
  (:use [retrospect.logging])
  (:use [retrospect.random])
  (:use [retrospect.state])
  (:use [retrospect.utility]))

(defrecord Hypothesis
    [id name type subtype apriori needs-explainer? conflicts?-fn
     explains short-str desc data]
  Object
  (toString [self] (format "%s(%s)" name short-str))
  Comparable
  (compareTo [self other] (compare (hash self) (hash other))))

(defn new-hyp
  [prefix type subtype apriori needs-explainer? conflicts?-fn
   explains short-str desc data]
  (prof :new-hyp
        (let [id (inc last-id)]
          (set-last-id id)
          (assoc
              (merge (Hypothesis.
                      id (format "%s%d" prefix id)
                      type subtype apriori needs-explainer? conflicts?-fn
                      explains short-str desc data)
                     data)
            :contents (assoc data :type type :subtype subtype)))))

(defmethod print-method Hypothesis
  [h w]
  (print-simple (format "%s(%s)/%.2f" (:name h) (:short-str h) (:apriori h)) w))

(def empty-workspace
  {:prior-workspace nil
   ;; workspace depth
   :depth 0
   ;; on every acceptance, save the delta; this is used to calculate doubt
   :acc-deltas []
   :graph (digraph)
   :oracle nil
   :cycle 0
   :log {:best [] :accrej {}}
   ;; keyed by hypid
   :hyp-log {}
   ;; keyed by hyp-id, records what each hyp explains
   :explains {}
   ;; all explainers, keyed by hyp-id, with vals as sets of hyp-ids; serves as a cache
   :explainers {}
   ;; a set of hypids that, should a hyp in here be accepted, needs to be explained
   :needs-explanation #{}
   ;; a map of hyp-id => set of hypids
   :hyp-ids {}
   ;; a map of type => seq
   :hypotheses {}
   ;; :data + :type map keys => hyp-id values (for dup searching)
   :hyp-contents {}
   ;; a map of type => seq, with additional key :all
   :accepted {:all #{}}
   ;; a map of hypid => set of hypids
   :sorted-explainers {}
   ;; a seq of hypids
   :sorted-explainers-explained '()
   ;; do the sorted explainers need to be udpated?
   :dirty false})

(defn lookup-hyp
  [workspace id]
  (prof :lookup-hyp
        (get-in workspace [:hyp-ids id])))

(defn accepted?
  [workspace hyp]
  (prof :accepted?
        ((get-in workspace [:accepted :all] #{}) (:id hyp))))

(defn hyp-log
  [workspace hyp]
  (get (:hyp-log workspace) (:id hyp)))

(defn explains
  "TODO: Fix for transitive explanation"
  [workspace hyp]
  (doall (filter identity (map #(lookup-hyp workspace %) (get (:explains workspace) (:id hyp))))))

(defn hyp-better-than?
  [workspace hyp1 hyp2]
  (let [conf (> (double (- (:apriori hyp1) (:apriori hyp2))) 0.001)
        ;; TODO: Fix for transitive explanation
        expl (> (count (filter (fn [hyp-id] (some #(= % hyp-id)
                                         (:sorted-explainers-explained workspace)))
                          (get (:explains workspace) (:id hyp1))))
                (count (filter (fn [hyp-id] (some #(= % hyp-id)
                                         (:sorted-explainers-explained workspace)))
                          (get (:explains workspace) (:id hyp2)))))
        explainers (> (count (get-in workspace [:explainers (:id hyp1)] []))
                      (count (get-in workspace [:explainers (:id hyp2)] [])))]
    {:conf conf :expl expl :explainers explainers}))

(defn compare-hyps
  "Since we are using probabilities, smaller value = less
   confidence. We want most confident first. With equal confidences,
   we look for higher explanatory power (explains more). If all that
   fails, comparison is done by the :id's (to keep it deterministic)."
  [workspace hyp1 hyp2]
  (let [comp1 (hyp-better-than? workspace hyp1 hyp2)
        comp2 (hyp-better-than? workspace hyp2 hyp1)]
    (cond (:conf comp1) -1
          (:conf comp2) 1
          (:expl comp1) -1
          (:expl comp2) 1
          (:explainers comp1) -1
          (:explainers comp2) 1
          :else (compare (:id hyp1) (:id hyp2)))))

(defn compare-by-delta
  [workspace {hyp1 :hyp expl1 :expl} {hyp2 :hyp expl2 :expl}]
  (prof :compare-by-delta
        (let [delta-fn (fn [hyps]
                         (let [normalized-aprioris (let [aprioris (map :apriori hyps)
                                                         s (reduce + aprioris)]
                                                     (if (= 0 s) aprioris
                                                         (map #(/ % s) aprioris)))]
                           (if (second normalized-aprioris)
                             (- (first normalized-aprioris) (second normalized-aprioris))
                             1.0)))
              expl1-delta (delta-fn expl1)
              expl2-delta (delta-fn expl2)]
          (comment
            (println (:id hyp1) (map :id expl1) (delta-fn expl1))
            (println (:id hyp2) (map :id expl2) (delta-fn expl2))
            (println "--"))
          (if (= 0 (compare expl1-delta expl2-delta))
            (if (= 0 (compare (:apriori (first expl1))
                              (:apriori (first expl2))))
              (compare (:id (first expl1)) (:id (first expl2)))
              (- (compare (:apriori (first expl1))
                          (:apriori (first expl2)))))
            (- (compare expl1-delta expl2-delta))))))

(defn sort-explainers
  [workspace explainers]
  (prof :sort-explainers
        (let [hyp-sorter (cond (= (:HypPreference params) "abd")
                               #(sort (partial compare-hyps workspace) %)
                               (= (:HypPreference params) "arbitrary")
                               #(my-shuffle %))
              combined-sorter (fn [expl1 expl2]
                                (let [hyp-apriori (- (compare (:apriori (:hyp expl1))
                                                              (:apriori (:hyp expl2))))]
                                  (if (= 0 hyp-apriori)
                                    (compare-by-delta workspace expl1 expl2)
                                    hyp-apriori)))
              expl-sorter (cond (= (:ContrastPreference params) "apriori,delta")
                                (fn [hs] (sort combined-sorter hs))
                                (= (:ContrastPreference params) "delta")
                                (fn [hs] (sort #(compare-by-delta workspace %1 %2) hs))
                                (= (:ContrastPreference params) "arbitrary")
                                #(my-shuffle %))]
          (expl-sorter (doall (map #(update-in % [:expl] hyp-sorter) explainers))))))

(defn update-sorted-explainers
  [workspace]
  (prof :update-sorted-explainers
        (do
          (log "Updating sorted explainers.")
          (let [expls (doall
                       (filter (comp first :expl)
                          (map (fn [hypid]
                               {:hyp (lookup-hyp workspace hypid)
                                :expl (doall
                                       (map #(lookup-hyp workspace %)
                                          (get-in workspace [:sorted-explainers hypid])))})
                             (:sorted-explainers-explained workspace))))
                expls-sorted (sort-explainers workspace expls)]
            (reduce (fn [ws {h :hyp expl :expl}]
                 (assoc-in ws [:sorted-explainers (:id h)] (doall (map :id expl))))
               (assoc workspace :sorted-explainers-explained
                      (doall (map (comp :id :hyp) expls-sorted))
                      :dirty false)
               expls-sorted)))))

(defn find-conflicts-all
  [workspace hyp]
  (prof :find-conflicts-all
        (if (nil? (:conflicts?-fn hyp)) []
            (doall (filter #((:conflicts?-fn hyp) hyp %) (vals (:hyp-ids workspace)))))))

(defn find-conflicts
  [workspace hyp]
  (prof :find-conflicts
        (doall (filter #((:conflicts?-fn hyp) hyp %)
                  (map #(lookup-hyp workspace %)
                     (set (apply concat (vals (:sorted-explainers workspace)))))))))

(defn dissoc-needing-explanation
  [workspace hyps]
  (prof :dissoc-needing-explanation
        (reduce (fn [ws h]
             (log "Dissociating" h "as needing explanation.")
             (-> ws (assoc :sorted-explainers-explained
                     (filter #(not= (:id h) %)
                        (:sorted-explainers-explained ws)))
                (update-in [:sorted-explainers] dissoc (:id h))))
           workspace hyps)))

(defn assoc-needing-explanation
  [workspace hyp]
  (prof :assoc-needing-explanation
        (let [expls (get-in workspace [:explainers (:id hyp)])]
          (log "Associating" hyp "as now needing explanation.")
          (-> workspace
             (update-in [:sorted-explainers-explained] conj (:id hyp))
             ;; put the key in even if expls is empty
             (assoc-in [:explainers (:id hyp)] expls)
             (assoc-in [:sorted-explainers (:id hyp)] expls)
             (assoc :dirty true)))))

(defn assoc-explainer
  [workspace hyp]
  (prof :assoc-explainer
        (reduce (fn [ws h]
             (log "Associating" hyp "as explainer of" h)
             (-> ws
                ;; add to explainers cache
                (update-in [:explainers (:id h)] conjs (:id hyp))
                ;; add to active explainers
                (update-in [:sorted-explainers (:id h)] conjs (:id hyp))))
           (assoc workspace :dirty true) (explains workspace hyp))))

(defn dissoc-explainer
  "Given some hypothesis, dissociate it as a potential explainer for
   anything."
  [workspace hyp]
  (prof :dissoc-explainer
        (do
          (log (format "Dissociating %s as an explainer" (str hyp)))
          (reduce (fn [ws h] (assoc-in ws [:sorted-explainers (:id h)]
                                 (filter #(not= (:id hyp) %)
                                    (get-in ws [:sorted-explainers (:id h)]))))
             workspace (explains workspace hyp)))))

(defn reject-many
  "Each 'rejected' hypothesis will be removed from the list of hyps
   that need to be explained, and any other hyps a rejected hyp
   explains will be marked as no longer potentially explained by the
   rejected hyp."
  [workspace hyps]
  (prof :reject-many
        (reduce
         (fn [ws hyp]
           (log "Rejecting" hyp)
           (let [ws2 (-> (dissoc-in ws [:sorted-explainers (:id hyp)])
                        (dissoc-explainer hyp))]
             (if @batch ws2
                 (update-in ws2 [:hyp-log (:id hyp)] conj
                            (format "Rejected in cycle %d" (:cycle workspace))))))
         workspace hyps)))

(defn record-if-needs-explanation
  [workspace hyp]
  (if (:needs-explainer? hyp)
    (do
      (log "Recording" hyp "as needing explanation.")
      (update-in workspace [:needs-explanation] conj (:id hyp)))
    workspace))

(defn add
  [workspace hyp]
  (prof :add
        (do
          (log "Adding" hyp)
          (cond (and (:conflicts?-fn hyp)
                     (some (fn [hyp2] ((:conflicts?-fn hyp) hyp hyp2))
                        (map #(lookup-hyp workspace %)
                           (:all (:accepted workspace)))))
                ;; hyp is in conflict; don't add it
                (do
                  (log "Not adding hyp because it has conflicts")
                  workspace)
                (< (:apriori hyp) (/ (double (:MinApriori params)) 100.0))
                (do (log "Not adding hyp because" (:apriori hyp)
                         "is below :MinApriori" (/ (double (:MinApriori params)) 100.0))
                    workspace)
                :else
                (if-let [prior-hyp-id (get (:hyp-contents workspace) (:contents hyp))]
                  ;; hyp already present; update explains in case it changed,
                  ;; and whether it needs explanation or not
                  (let [prior-hyp (lookup-hyp workspace prior-hyp-id)]
                    (log hyp "already in workspace as" prior-hyp
                         "-- updating what it explains")
                    (-> workspace 
                       (update-in [:explains prior-hyp-id]
                                  set/union (set (map #(get (:hyp-contents workspace) %)
                                                    (:explains hyp))))
                       (record-if-needs-explanation (assoc prior-hyp :needs-explainer?
                                                           (:needs-explainer? hyp)))
                       (assoc-explainer (assoc prior-hyp :explains (:explains hyp)))))
                  ;; otherwise, add the new hyp
                  (let [hyp-apriori (if ((:oracle-types workspace) (:type hyp))
                                      (if ((:oracle workspace) hyp)
                                        (assoc hyp :apriori 1.0)
                                        (assoc hyp :apriori 0.0))
                                      (if (:UseScores params) hyp (assoc hyp :apriori 1.0)))]
                    (prof :add-ws-update
                          (-> workspace
                             (assoc-in [:hyp-ids (:id hyp-apriori)] hyp-apriori)
                             (assoc-in [:hyp-contents (:contents hyp-apriori)]
                                       (:id hyp-apriori))
                             (assoc-in [:explains (:id hyp-apriori)]
                                       (set (map #(get (:hyp-contents workspace) %)
                                               (:explains hyp-apriori))))
                             (record-if-needs-explanation hyp-apriori)
                             (assoc-explainer hyp-apriori)
                             (update-in [:hypotheses (:type hyp-apriori)]
                                        conj (:id hyp-apriori))))))))))

(defn accept
  [workspace hyp nbest explained delta comparison]
  (prof :accept
        ;; don't "accept" a hyp that was never added (due to
        ;; conflicts); this probably only matters in (add-observation)
        (if (nil? (get (:hyp-ids workspace) (:id hyp))) workspace
            (let [ws-acc (prof :accept-update
                               (-> workspace
                                  (update-in [:accepted (:type hyp)] conj (:id hyp))
                                  (update-in [:accepted :all] conj (:id hyp))))
                  ws-hyplog (if @batch ws-acc
                                (update-in ws-acc [:hyp-log (:id hyp)] conj
                                           (format (str "Accepted in cycle %d "
                                                   "to explain %s with delta %.2f "
                                                   "(essential? %s; next-best: %s); "
                                                   "comparison: %s")
                                              (:cycle workspace)
                                              explained delta (nil? nbest) nbest
                                              comparison)))
                  ws-expl (dissoc-needing-explanation ws-hyplog (explains workspace hyp))
                  conflicts (prof :accept-conflicts
                                  (when (:conflicts?-fn hyp)
                                    (find-conflicts ws-expl hyp)))
                  ws-conflicts (if conflicts
                                 (prof :accept-reject-many
                                       (reject-many ws-expl conflicts))
                                 ws-expl)
                  ws-needs-exp (if-not ((:needs-explanation ws-conflicts) (:id hyp))
                                 ws-conflicts
                                 (prof :accept-needs-exp
                                       (assoc-needing-explanation ws-conflicts hyp)))]
              (prof :accept-final
                    (update-in ws-needs-exp [:log :accrej (:cycle ws-needs-exp)] conj
                               {:acc hyp :rej conflicts}))))))

(defn add-observation
  [workspace hyp]
  (-> workspace (add hyp) (accept hyp nil [] 0.0 {})))

(defn get-unexplained
  [workspace]
  (:sorted-explainers-explained workspace))

(defn get-unexp-pct
  [workspace]
  (prof :get-unexp-pct
        (let [unexp (get-unexplained workspace)]
          (if (empty? unexp) 0.0
              (/ (double (count unexp))
                 (double (count (filter (:needs-explanation workspace)
                                   (:all (:accepted workspace))))))))))

(defn get-no-explainers
  [workspace]
  (prof :find-no-explainers
        (filter (fn [hyp-id] (empty? (get (:explainers workspace) hyp-id)))
           (filter (:needs-explanation workspace)
              (:all (:accepted workspace))))))

(defn get-noexp-pct
  [workspace]
  (prof :get-noexp-pct
        (let [noexp (get-no-explainers workspace)]
          (if (empty? noexp) 0.0
              (/ (double (count noexp))
                 (double (count (filter (:needs-explanation workspace)
                                   (:all (:accepted workspace))))))))))

(defn calc-doubt
  [workspace]
  (prof :calc-doubt
        (let [acc (set/difference (:all (:accepted workspace))
                                  (:observation (:accepted workspace)))]
          (if (empty? acc) 0.0
              (/ (reduce + (map #(- 1.0 (:apriori (lookup-hyp workspace %))) acc))
                 (double (count acc)))))))

(comment
  (if (empty? (:acc-deltas workspace)) 0.0
            (/ (reduce + (map #(- 1.0 %) (:acc-deltas workspace)))
               (count (:acc-deltas workspace)))))

(defn calc-coverage
  [workspace]
  (prof :calc-coverage
        (let [acc-needs-exp (set (filter (:needs-explanation workspace)
                                    (:all (:accepted workspace))))]
          (if (empty? acc-needs-exp) 1.0
              (let [accessible (mapcat (fn [hyp-id] (pre-traverse (:graph workspace) hyp-id))
                                       acc-needs-exp)]
                (/ (double (count (set/intersection acc-needs-exp (set accessible))))
                   (double (count acc-needs-exp))))))))

(defn find-unaccepted
  [workspace]
  (set/difference
   (set (apply concat (vals (:hypotheses workspace))))
   (set (:all (:accepted workspace)))))

(defn find-best
  [workspace]
  (prof :find-best
        (let [not-empty-explained (filter #(not-empty (get (:sorted-explainers workspace) %))
                                     (:sorted-explainers-explained workspace))]
          (when (not-empty not-empty-explained)
            (let [essentialid
                  (first (filter #(nil? (second (get (:sorted-explainers workspace) %)))
                            not-empty-explained))]
              (if essentialid
                (let [essential (lookup-hyp workspace essentialid) 
                      bestid (first (get (:sorted-explainers workspace) essentialid))
                      best (lookup-hyp workspace bestid)]
                  {:best best :explained essential :alts [] :comparison {} :delta 1.0})
                ;; otherwise, choose highest-delta non-essential
                (let [explid (first not-empty-explained)
                      expl (lookup-hyp workspace explid)
                      choices (doall (map #(lookup-hyp workspace %)
                                        (get (:sorted-explainers workspace) explid)))
                      best (first choices)
                      nbest (second choices)
                      normalized-aprioris (let [aprioris (map :apriori choices)
                                                s (reduce + aprioris)]
                                            (if (= 0 s) aprioris
                                                (map #(/ % s) aprioris)))
                      delta (- (first normalized-aprioris) (second normalized-aprioris))
                      comparison (hyp-better-than? workspace best nbest)]
                  (log "best:" best "nbest:" nbest "delta:" delta)
                  (when (or (= 0 (:Threshold params))
                            (>= delta (+ 0.001 (/ (:Threshold params) 100.0)))
                            ;; if threshold is not good enough,
                            ;; see if one hyp is better purely by
                            ;; other factors such as explanatory power
                            (and (:ConsiderExplPower params)
                                 (or (:expl comparison) (:explainers comparison))))
                    {:best best :nbest nbest :delta delta
                     :normalized-aprioris normalized-aprioris
                     :explained expl :alts (rest choices)
                     :comparison comparison}))))))))

(defn update-kb
  [workspace]
  (prof :update-kb
        (if-not (:UpdateKB params) workspace
                (let [new-kb-hyps ((:update-kb-fn (:abduction @problem))
                                   (:accepted workspace)
                                   (get-unexplained workspace)
                                   (:hypotheses workspace)
                                   (partial lookup-hyp workspace))]
                  (-> (reduce (fn [ws h] (assoc-in ws [:hyp-ids (:id h)] h))
                        workspace new-kb-hyps)
                     (assoc-in [:accepted :kb] (doall (map :id new-kb-hyps)))
                     (update-in [:accepted :all] set/union (set (map :id new-kb-hyps))))))))

(defn get-explaining-hypotheses
  [workspace time-now]
  (prof :get-explaining-hypotheses
        ((:hypothesize-fn (:abduction @problem))
         (map #(lookup-hyp workspace %) (:sorted-explainers-explained workspace))
         (:accepted workspace)
         (partial lookup-hyp workspace) time-now)))

(defn update-graph
  "Need to run this before evaluation in order to calculate coverage."
  [workspace]
  (prof :update-graph
        (let [g-added (reduce (fn [g h]
                           (-> g (add-nodes (:id h))
                              (add-attr (:id h) :id (:id h))
                              (add-attr (:id h) :label (:short-str h))))
                         (digraph)
                         (vals (:hyp-ids workspace)))
              g-expl (reduce (fn [g h] (reduce (fn [g2 e]
                                      (add-edges g2 [(:id h) (:id e)]))
                                    g (explains workspace h)))
                        g-added (vals (:hyp-ids workspace)))
              ;; finding conflicts takes a long time, so only do this
              ;; if not in batch mode
              g-conflicts (if @batch g-expl
                              (reduce
                               (fn [g h]
                                 (let [conflicts (find-conflicts-all
                                                  workspace (lookup-hyp workspace h))]
                                   (reduce (fn [g2 c]
                                        (if (or (has-edge? g2 h c) (has-edge? g2 c h)) g2
                                            (-> g2 (add-edges [h c])
                                               (add-attr h c :dir "none")
                                               (add-attr h c :style "dotted")
                                               (add-attr h c :constraint false))))
                                      g (map :id conflicts))))
                               g-expl (apply concat (vals (:hypotheses workspace)))))
              g-accepted (reduce
                          (fn [g h]
                            (-> g (add-attr h :fontcolor "green")
                               (add-attr h :color "green")))
                          g-conflicts (:all (:accepted workspace)))]
          (assoc workspace :graph g-accepted))))

(defn explain
  [workspace]
  (prof :explain
        ;; need (loop) because we are using (recur) which isn't going
        ;; to work when profiling is on
        (loop [workspace (-> workspace (assoc :log (:log empty-workspace)
                                             :acc-deltas [])
                            (update-graph))]
          (log "Explaining again...")
          (let [ws-explainers (if true #_(:dirty workspace)
                                (update-sorted-explainers workspace)
                                workspace)]
            (log "Explainers:" (:sorted-explainers ws-explainers)
                 (:sorted-explainers-explained ws-explainers))
            (if (empty? (:sorted-explainers-explained ws-explainers))
              (do (log "No explainers. Done.")
                  ws-explainers)
              (let [{:keys [best nbest explained delta comparison] :as b}
                    (find-best ws-explainers)]
                (if-not best
                  (do (log "No best. Done.") ws-explainers)
                  (do (log "Best is" (:id best) (:apriori best))
                      (let [ws-accepted
                            (let [ws-logged (-> ws-explainers
                                               (update-in [:acc-deltas] conj delta)
                                               (update-in [:cycle] inc)
                                               (update-in [:log :best] conj b))]
                              (accept ws-logged best nbest explained delta comparison))]
                        (recur ws-accepted))))))))))

(defn update-hypotheses
  [workspace time-now]
  (prof :update-hypotheses
        (let [hyps (get-explaining-hypotheses workspace time-now)]
          (reduce add workspace hyps))))

(defn add-sensor-hyps
  [workspace time-prev time-now sensors]
  (prof :add-sensor-hyps
        (let [hs ((:make-sensor-hyps-fn (:abduction @problem))
                  sensors time-prev time-now
                  (:accepted workspace) (partial lookup-hyp workspace))]
          (reduce add-observation workspace hs))))

(defn add-kb
  [workspace hyps]
  (prof :add-kb
        (reduce (fn [ws h]
             (-> ws (add h)
                (update-in [:accepted (:type h)] conj (:id h))
                (update-in [:accepted :all] conj (:id h))))
           workspace hyps)))

(defn remove-kb
  [workspace]
  (prof :remove-kb
        (let [kb-hyps (doall (map #(lookup-hyp workspace %)
                                (get-in workspace [:hypotheses :kb])))]
          (-> (reduce (fn [ws h] (dissoc-in ws [:hyp-contents (:contents h)]))
                workspace kb-hyps)
             (assoc-in [:accepted :kb] '())
             (assoc-in [:hypotheses :kb] '())))))

(defn init-kb
  [workspace training]
  (prof :init-kb
        (add-kb workspace ((:generate-kb-fn (:abduction @problem)) training))))

(defn init-workspace
  []
  (prof :init-workspace
        (assoc empty-workspace
          :oracle-types
          (set (map keyword (str/split (:Oracle params) #","))))))

(defn reset-workspace
  [workspace]
  (prof :reset-workspace
        (add-kb (assoc empty-workspace :oracle (:oracle workspace)
                       :oracle-types (:oracle-types workspace)
                       :cycle (:cycle workspace))
                (doall (map #(lookup-hyp workspace %)
                          (get-in workspace [:accepted :kb]))))))

(defn revert-workspace
  ([workspace depth]
     (if (> depth (:depth workspace)) workspace
         (loop [ws workspace]
           (cond (= depth (:depth ws)) ws
                 (nil? (:prior-workspace ws)) (reset-workspace workspace)
                 :else
                 (recur (:prior-workspace ws))))))
  ([workspace] (revert-workspace workspace (dec (:depth workspace)))))

(defn workspace-depth
  [workspace]
  (loop [ws workspace i 0]
    (if-let [p (:prior-workspace ws)]
      (recur p (inc i)) i)))

