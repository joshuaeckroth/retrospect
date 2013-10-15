(ns retrospect.reason.abduction.workspace
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:use [plumbing.core])
  (:use [loom.graph :only
         [digraph nodes incoming neighbors weight
          add-nodes add-edges remove-nodes edges remove-edges
          has-edge? transpose]])
  (:use [loom.io :only [view]])
  (:use [loom.alg :only [topsort]])
  (:use [loom.alg-generic :only [bf-traverse]])
  (:use [loom.attr :only [add-attr remove-attr attr]])
  (:use [retrospect.logging])
  (:use [retrospect.evaluate :only [avg]])
  (:use [geppetto.random])
  (:use [retrospect.state]))

(def calls-to-observe (atom {}))
(def calls-to-hypothesize (atom {}))

(defrecord Hypothesis
    [id name type subtype apriori needs-explainer? conflicts-tags conflicts?-fn
     explains short-str desc data]
  Object
  (toString [self] (format "%s(%s)/%.2f" name short-str apriori))
  Comparable
  (compareTo [self other] (compare (hash self) (hash other))))

(defn new-hyp
  ;; contents not provided; make them
  ([prefix type subtype apriori needs-explainer? conflicts-tags conflicts?-fn
    explains short-str desc data]
   (new-hyp prefix type subtype apriori needs-explainer? conflicts-tags conflicts?-fn
            explains short-str desc data
            (assoc data :type type :subtype subtype)))
  ;; contents provided; allows the hyp generator to decide what constitutes
  ;; an identical hyp (note that :data will be copied from new hyp
  ;; when an identical hyp is detected)
  ([prefix type subtype apriori needs-explainer? conflicts-tags conflicts?-fn
    explains short-str desc data contents]
     (let [id (inc last-id)]
       (set-last-id id)
       (assoc (merge (Hypothesis.
                      id (format "%s%d" prefix id)
                      type subtype apriori needs-explainer? conflicts-tags conflicts?-fn
                      explains short-str desc data)
                     data)
         :contents contents))))

(defn new-composite
  [prefix type subtype apriori explains short-str desc data hyps]
  (let [hyp (new-hyp prefix type subtype apriori false nil nil
                     explains short-str desc data)]
    (assoc hyp :composite? true :hyps hyps)))

(defmethod print-method Hypothesis
  [h w]
  (print-simple (format "%s(%s)/%.2f" (:name h) (:short-str h) (:apriori h)) w))

(def empty-workspace
  {:oracle nil
   :meta-oracle nil
   ;; what was accepted, rejected, merged with the 'best' map
   :accrej {}
   ;; hypgraph
   :hypgraph (digraph)
   ;; acceptance graph, which stores the dependencies of acceptances
   :accgraph (digraph)
   ;; a cache
   :accepted #{}
   ;; a cache
   :rejected #{}
   ;; a cache
   :unexplained #{}
   ;; a map of hyp-id => hyp
   :hyp-ids {}
   ;; a map of type => seq, with additional key :all
   :hypotheses {:all #{}}
   ;; :data + :type map keys => hyp-id values (for dup searching)
   :hyp-contents {}
   ;; a cache
   :composites #{}
   ;; a cache: tag => seq of hypids
   :conflicts-tag-map {}})

(defn lookup-hyp
  [workspace id]
  (get-in workspace [:hyp-ids id]))

(defn accepted?
  [workspace h]
  (let [hypid (if (integer? h) h (:id h))]
    ((:accepted workspace) hypid)))

(defn accepted
  [workspace]
  (let [all (doall (map #(lookup-hyp workspace %) (:accepted workspace)))]
    (merge (group-by :type all) {:all all})))

(defn hypotheses
  [workspace]
  (let [g (:hypgraph workspace)
        ;; intentionally lazy (not sure if it helps, since using group by below)
        all (map #(lookup-hyp workspace %) (nodes g))]
    (merge (group-by :type all) {:all all})))

(defn composite-hypotheses
  [workspace]
  (map #(lookup-hyp workspace %) (:composites workspace)))

(defn accepted-cycle
  [workspace h]
  (let [hypid (if (integer? h) h (:id h))]
    (attr (:hypgraph workspace) hypid :accepted-cycle)))

(defn accepted-explained
  [workspace h]
  (let [hypid (if (integer? h) h (:id h))]
    (attr (:hypgraph workspace) hypid :accepted-explained)))

(defn accepted-rivals
  [workspace h]
  (let [hypid (if (integer? h) h (:id h))]
    (attr (:hypgraph workspace) hypid :accepted-rivals)))

(defn rejected?
  [workspace h]
  (let [hypid (if (integer? h) h (:id h))]
    ((:rejected workspace) hypid)))

(defn rejected
  [workspace]
  (let [all (doall (map #(lookup-hyp workspace %) (:rejected workspace)))]
    (merge (group-by :type all) {:all all})))

(defn rejected-cycle
  [workspace h]
  (let [hypid (if (integer? h) h (:id h))]
    (attr (:hypgraph workspace) hypid :rejected-cycle)))

(defn rejection-reason
  [workspace h]
  (let [hypid (if (integer? h) h (:id h))]
    (attr (:hypgraph workspace) hypid :rejection-reason)))

(defn prevent-rejection
  [workspace h rej-reason]
  (let [hypid (if (integer? h) h (:id h))]
    (update-in workspace [:hypgraph]
               add-attr hypid :prevent-rejection rej-reason)))

(defn prevented-rejection?
  [workspace h rej-reason]
  (let [hypid (if (integer? h) h (:id h))]
    (= rej-reason (attr (:hypgraph workspace) hypid :prevent-rejection))))

(defn undecided?
  [workspace h]
  (and (not (accepted? workspace h))
       (not (rejected? workspace h))))

(defn undecided
  [workspace]
  (let [g (:hypgraph workspace)]
    (doall (sort-by :id (map #(lookup-hyp workspace %)
                             (set/difference (nodes g) (:accepted workspace) (:rejected workspace)))))))

(defn prevent-undecide
  [workspace h]
  (let [hypid (if (integer? h) h (:id h))]
    (update-in workspace [:hypgraph]
               add-attr hypid :prevent-undecide true)))

(defn undeciding-prevented?
  [workspace h]
  (let [hypid (if (integer? h) h (:id h))]
    (attr (:hypgraph workspace) hypid :prevent-undecide)))

(defn explains?
  "Does h1 explain h2?"
  [workspace h1 h2]
  (let [hypid1 (if (integer? h1) h1 (:id h1))
        hypid2 (if (integer? h2) h2 (:id h2))]
    (has-edge? (:hypgraph workspace) hypid1 hypid2)))

(defn explains
  [workspace h]
  (let [hypid (if (integer? h) h (:id h))]
    (doall (map #(lookup-hyp workspace %) (neighbors (:hypgraph workspace) hypid)))))

(defn explainers
  [workspace h]
  (let [hypid (if (integer? h) h (:id h))]
    (doall (map #(lookup-hyp workspace %) (incoming (:hypgraph workspace) hypid)))))

(defn needs-explainer?
  [workspace h]
  (if (integer? h)
    (:needs-explainer? (lookup-hyp workspace h))
    (:needs-explainer? h)))

(defn unexplained?
  [workspace h]
  (let [hypid (if (integer? h) h (:id h))]
    (and ((:unexplained workspace) hypid)
         (accepted? workspace hypid)
         (needs-explainer? workspace hypid)
         (not-any? (fn [h2] (accepted? workspace h2)) (explainers workspace hypid)))))

(defn unexplained
  [workspace]
  (doall (map #(lookup-hyp workspace %)
              (filter #(unexplained? workspace %) (:unexplained workspace)))))

(defn no-explainers?
  [workspace h]
  (let [hypid (if (integer? h) h (:id h))
        expls (explainers workspace hypid)]
    (and (unexplained? workspace hypid)
         (or (empty? expls)
             (every? (fn [expl] (and (rejected? workspace expl)
                                     (not= :ignoring (rejection-reason workspace expl))))
                     expls)))))

(defn no-explainers
  [workspace]
  (doall (map #(lookup-hyp workspace %) (filter #(no-explainers? workspace %)
                                                (:unexplained workspace)))))

(defn hyp-log
  [workspace h]
  (let [hypid (if (integer? h) h (:id h))]
    (attr (:hypgraph workspace) hypid :log)))

(defn add-to-hyp-log
  [workspace h msg]
  (let [hypid (if (integer? h) h (:id h))
        new-log (format "%s%s\n" (or (attr (:hypgraph workspace) hypid :log) "") msg)]
    (update-in workspace [:hypgraph] add-attr hypid :log new-log)))

(defn clear-hyp-log
  "Primarily for test cases."
  [workspace h]
  (let [hypid (if (integer? h) h (:id h))]
    (update-in workspace [:hypgraph] remove-attr hypid :log)))

(defn clear-all-hyp-logs
  "Primarily for test cases."
  [workspace]
  (reduce clear-hyp-log workspace (:all (hypotheses workspace))))

(defn conflicts?
  [h1 h2]
  (if (= h1 h2) false
      (if-let [c? (or (get-in @cache [:conflicts (:simulation params) (:id h1) (:id h2)])
                      (get-in @cache [:conflicts (:simulation params) (:id h2) (:id h1)]))]
        c?
        (let [c? (cond (:composite? h2)
                       (if (some (fn [h] ((:conflicts?-fn h) h1 h)) (:hyps h2)) true false)
                       (:composite? h1)
                       (if (some (fn [h] ((:conflicts?-fn h) h h2)) (:hyps h1)) true false)
                       (or (nil? (:conflicts?-fn h1)) (nil? (:conflicts?-fn h2)))
                       false
                       :else
                       (if ((:conflicts?-fn h1) h1 h2) true false))]
          (swap! cache
                 #(-> %
                      (assoc-in [:conflicts (:simulation params) (:id h1) (:id h2)] c?)
                      (assoc-in [:conflicts (:simulation params) (:id h2) (:id h1)] c?)))
          c?))))

(defn find-conflicts
  [workspace hyp]
  (filter #(conflicts? hyp %)
          (map #(lookup-hyp workspace %)
               (set (mapcat #(get-in workspace [:conflicts-tag-map %])
                            (if (:composite? hyp)
                              (set (mapcat :conflicts-tags (:hyps hyp)))
                              (:conflicts-tags hyp)))))))

(defn conflicts-with-accepted?
  [workspace hyp]
  (some (fn [hyp2] (conflicts? hyp hyp2))
        (map #(lookup-hyp workspace %)
             (filter (fn [hypid] (accepted? workspace hypid))
                     (set (mapcat #(get-in workspace [:conflicts-tag-map %])
                                  (if (:composite? hyp)
                                    (set (mapcat :conflicts-tags (:hyps hyp)))
                                    (:conflicts-tags hyp))))))))

(defn related-hyps
  ;; includes this hyp
  ([workspace hyp]
     (let [g (transpose (:hypgraph workspace))
           acc-rej (concat (:all (accepted workspace))
                           (filter #(= :conflict (rejection-reason workspace %))
                                   (:all (rejected workspace))))]
       (related-hyps workspace g hyp acc-rej)))
  ([workspace g hyp acc-rej]
     (bf-traverse (fn [hypid]
                    (let [h (lookup-hyp workspace hypid)]
                      (filter #(not= :observation (:type (lookup-hyp workspace %)))
                              (concat (filter #(not (undecided? workspace %))
                                              (neighbors g hypid))
                                      (map :id (filter #(conflicts? % h) acc-rej))))))
                  (:id hyp))))

(defn related-hyps?
  [workspace hyp1 hyp2]
  (not-empty (set/intersection (set (related-hyps workspace hyp1))
                    (set (related-hyps workspace hyp2)))))

(defn undecide
  ([workspace hyp cycle]
     (undecide workspace hyp cycle true))
  ([workspace hyp cycle undecide-related?]
     ;; note related-hyps includes hyp itself
     (let [rel-hyps (if undecide-related?
                      (filter #(not (undeciding-prevented? workspace %))
                              (related-hyps workspace hyp))
                      [(:id hyp)])]
       (log "Undeciding" hyp "and related hyps" rel-hyps)
       (reduce (fn [ws hypid]
                 (let [hyp (lookup-hyp ws hypid)]
                   (-> ws
                       (update-in [:hypgraph] remove-attr hypid :accepted?)
                       (update-in [:hypgraph] remove-attr hypid :accepted-cycle)
                       (update-in [:hypgraph] remove-attr hypid :accepted-explained)
                       (update-in [:hypgraph] remove-attr hypid :accepted-newly-explained)
                       (update-in [:hypgraph] remove-attr hypid :rejected?)
                       (update-in [:hypgraph] remove-attr hypid :rejected-cycle)
                       (update-in [:hypgraph] remove-attr hypid :rejection-reason)
                       (update-in [:accepted] disj hypid)
                       (update-in [:rejected] disj hypid)
                       (?> (:needs-explainer? hyp) update-in [:unexplained] conj (:id hyp))
                       (update-in [:unexplained] set/union (set (attr (:hypgraph workspace)
                                                                      hypid :accepted-newly-explained)))
                       (add-to-hyp-log hypid (format "Undecided at cycle %d" cycle)))))
               workspace rel-hyps))))

(defn unreject
  [workspace hyp]
  (-> workspace
      (update-in [:hypgraph] remove-attr (:id hyp) :rejected?)
      (update-in [:hypgraph] remove-attr (:id hyp) :rejected-cycle)
      (update-in [:hypgraph] remove-attr (:id hyp) :rejection-reason)
      (update-in [:rejected] disj (:id hyp))))

;; forward declaration so (reject) can refer to (add) and vice versa
(declare add)

(defn reject
  "Each 'rejected' hypothesis will be removed from the list of hyps
   that need to be explained, and any other hyps a rejected hyp
   explains will be marked as no longer potentially explained by the
   rejected hyp."
  [workspace hyp reason-tag cycle]
  (if (rejected? workspace hyp) workspace
      (do
        (log "Rejecting" hyp "with reason" reason-tag)
        (let [ws (-> workspace
                     (update-in [:hypgraph] remove-attr (:id hyp) :accepted?)
                     (update-in [:hypgraph] remove-attr (:id hyp) :accepted-cycle)
                     (update-in [:hypgraph] remove-attr (:id hyp) :accepted-explained)
                     (update-in [:hypgraph] remove-attr (:id hyp) :accepted-newly-explained)
                     (update-in [:hypgraph] add-attr (:id hyp) :rejected? true)
                     (update-in [:hypgraph] add-attr (:id hyp) :rejected-cycle cycle)
                     (update-in [:hypgraph] add-attr (:id hyp) :rejection-reason reason-tag)
                     (update-in [:accepted] disj (:id hyp))
                     (update-in [:rejected] conj (:id hyp))
                     (update-in [:unexplained] disj (:id hyp))
                     (update-in [:accrej :rej] conj (:id hyp)))
              ;; is this hyp a member of any composites? if so, those
              ;; composites need to be rejected as well
              composites (filter (fn [h] (some (fn [hc] (= hyp hc)) (:hyps h)))
                                 (composite-hypotheses workspace))
              ws-comp (reduce (fn [ws h] (reject ws h reason-tag cycle)) ws composites)]
          (add-to-hyp-log ws-comp hyp (format "%s rejected at cycle %d with reason %s" (str hyp) cycle (str reason-tag)))))))

(defn oracle-apriori
  [oracle-fn hyp meta?]
  (assoc hyp :apriori (if (oracle-fn hyp) 1.0 0.0)))

(defn update-hyp-apriori
  [workspace hyp]
  (let [hyp-s (cond
               ;; check oracle
               (and (:oracle workspace)
                    ((:oracle-types workspace) (:type hyp)))
               (oracle-apriori (:oracle workspace) hyp false)
               ;; check meta-oracle
               (and (:meta-oracle workspace)
                    ((:meta-oracle-types workspace) (:type hyp)))
               (oracle-apriori (:meta-oracle workspace) hyp true)
               :else
               (if (not (:UseScores params))
                 (assoc hyp :apriori 1.0)
                 (if (= 100 (:ScoreLevels params)) hyp
                     (let [levels (range 0.0 1.01 (/ 1.0 (double (dec (:ScoreLevels params)))))
                           apriori-new (first (sort-by #(Math/abs (- (:apriori hyp) %)) levels))]
                       (assoc hyp :apriori apriori-new)))))]
    (if (< (my-rand) (double (/ (:InvertScoresPct params) 100.0)))
      (do (log "Inverting score of" hyp)
          (assoc hyp-s :apriori (- 1.0 (:apriori hyp-s))))
      hyp-s)))

(defn add-helper
  [workspace hyp]
  (let [ ;; sometimes, an added hyp explains stuff not already added;
        ;; this can happen when a problem case is added during abductive metareasoning;
        ;; we just ignore these non-existing explained hyps
        explains (filter identity (map #(get (:hyp-contents workspace) %) (:explains hyp)))
        conflicts-tags (if (:composite? hyp)
                         (set (mapcat :conflicts-tags (:hyps hyp)))
                         (:conflicts-tags hyp))
        ws-conflicts-tags (reduce (fn [ws ct] (update-in ws [:conflicts-tag-map ct] conj (:id hyp)))
                                  workspace conflicts-tags)]
    (-> ws-conflicts-tags
        (assoc-in [:hyp-ids (:id hyp)] hyp)
        (assoc-in [:hyp-contents (:contents hyp)] (:id hyp))
        (update-in [:hypgraph] add-nodes (:id hyp))
        (update-in [:hypgraph] #(apply add-edges % (for [e explains] [(:id hyp) e])))
        (update-in [:hypotheses (:type hyp)] conj (:id hyp))
        (update-in [:hypotheses :all] conj (:id hyp))
        (?> (:composite? hyp) update-in [:composites] conj (:id hyp))
        (?> (:needs-explainer? hyp) update-in [:unexplained] conj (:id hyp)))))

(defn add-existing-hyp-rejected
  [workspace hyp]
  ;; if it was rejected due to :minscore and it would not again be
  ;; rejected for the same reason, unreject it
  (if (and (= :minscore (rejection-reason workspace hyp))
           (>= (:apriori hyp)
               (double (/ (:MinScore params) 100.0))))
    (do (log "...yet was rejected due to :minscore previously\n"
             "...but now satisfies minscore, so unrejecting.")
        (-> workspace
            (add-to-hyp-log hyp (format "%s Unrejecting because now satisfies MinScore." (str hyp)))
            (unreject hyp)))
    (do (log "...yet was rejected due to" (rejection-reason workspace hyp)
             "so leaving as is (not adding).")
        workspace)))

(defn add-existing-hyp-updated
  "hyp already present; update explains in case it changed,
   and whether it needs explanation or not"
  [workspace hyp prior-hyp-id cycle]
  (let [prior-hyp (lookup-hyp workspace prior-hyp-id)]
    (if (and (= (:apriori hyp) (:apriori prior-hyp))
             (= (:explains hyp) (:explains prior-hyp))
             (= (:needs-explainer? hyp) (:needs-explainer? prior-hyp)))
      ;; nothing changed in this hyp
      workspace
      ;; otherwise, there are changes in this version of the same hyp
      (let [prior-hyp-updated (assoc prior-hyp
                                :needs-explainer? (:needs-explainer? hyp)
                                :explains (set/union (set (:explains prior-hyp))
                                                     (set (:explains hyp)))
                                :apriori (:apriori prior-hyp)
                                :data (:data hyp)
                                :contents (assoc (:data hyp) :type (:type hyp) :subtype (:subtype hyp)))
            ;; ensure hyp is updated with new :explains, :apriori, etc.
            ws (add-helper workspace prior-hyp-updated)]
        (if (rejected? ws prior-hyp)
          ;; possibly unreject it
          (add-existing-hyp-rejected ws prior-hyp-updated)
          ws)))))

(defn add
  [workspace hyp cycle]
  (let [hyp-apriori (update-hyp-apriori workspace hyp)
        hyp-c (if (:composite? hyp-apriori)
                ;; update a composite hyp so that updated sub-hyps are used
                (let [lookup-sub-hyp (fn [h] (if-let [h-id (get (:hyp-contents workspace)
                                                                (:contents h))]
                                               (lookup-hyp workspace h-id) h))
                      updated-sub-hyps (map lookup-sub-hyp (:hyps hyp-apriori))]
                  (assoc hyp-apriori :hyps updated-sub-hyps))
                hyp-apriori)]
    (log "Adding" hyp-c)
    (let [ws (if-let [prior-hyp-id (get (:hyp-contents workspace) (:contents hyp-c))]
               (-> workspace
                   (add-to-hyp-log prior-hyp-id (format "%s Added as updated hyp %s at cycle %d (original: %s)" hyp-c prior-hyp-id cycle hyp))
                   (add-existing-hyp-updated hyp-c prior-hyp-id cycle))
               (-> workspace
                   (add-to-hyp-log hyp-c (format "%s Added at cycle %d (original: %s)" hyp-c cycle hyp))
                   (add-helper hyp-c)))]
      (if (and (not (rejected? ws hyp-c))
               (conflicts-with-accepted? ws hyp-c))
        (do (log (str "...yet it conflicts with an already accepted hyp, "
                      "so immediately rejecting."))
            (reject ws hyp-c :conflict cycle))
        ws))))

(defn accept
  [workspace hyp nbest alts explained delta comparison cycle]
  (do
    (log "Accepting" hyp)
    ;; don't "accept" a hyp that was never added
    (cond (nil? (get (:hyp-ids workspace) (:id hyp)))
          (if-let [prior-hyp-id (get (:hyp-contents workspace) (:contents hyp))]
            (accept workspace (lookup-hyp workspace prior-hyp-id)
                    nbest alts explained delta comparison cycle)
            (do (log "Cannot accept, never added.")
                workspace))
          (accepted? workspace hyp)
          (do (log "Already accepted.")
              workspace)
          (rejected? workspace hyp)
          (do (log "Already rejected, with reason" (rejection-reason workspace hyp))
              workspace)
          :else
          (let [newly-explained (filter #(unexplained? workspace %)
                                        (set/intersection (:unexplained workspace)
                                                          (set (map :id (explains workspace hyp)))))
                conflicts (if (= :observation (:type hyp)) []
                              (find-conflicts workspace hyp))
                ws-acc (-> workspace
                           (update-in [:hypgraph] add-attr (:id hyp)
                                      :accepted? true)
                           (update-in [:hypgraph] add-attr (:id hyp)
                                      :accepted-cycle cycle)
                           (update-in [:hypgraph] add-attr (:id hyp)
                                      :accepted-explained explained)
                           (update-in [:hypgraph] add-attr (:id hyp)
                                      :accepted-newly-explained newly-explained)
                           (update-in [:hypgraph] add-attr (:id hyp)
                                      :accepted-rivals alts)
                           (update-in [:accepted] conj (:id hyp))
                           (update-in [:unexplained] set/difference newly-explained))
                ws-hyplog (if @batch ws-acc
                              (add-to-hyp-log ws-acc hyp
                                              (format (str "Accepted at cycle %d to explain %s with delta %.2f "
                                                           "(essential? %s; next-best: %s); "
                                                           "comparison: %s")
                                                      cycle explained delta (nil? nbest) nbest
                                                      comparison)))
                ws-conflicts (reduce (fn [ws h] (if (undecided? ws h)
                                                  (reject ws h :conflict cycle)
                                                  ws))
                                     ws-hyplog conflicts)
                ws-composite (if (not (:composite? hyp)) ws-conflicts
                                 (reduce (fn [ws h]
                                           (let [ws-added (add ws h cycle)
                                                 ;; when added, hyp id may have
                                                 ;; changed; find the hyp again
                                                 h-updated (lookup-hyp ws-added
                                                                       (get (:hyp-contents ws-added)
                                                                            (:contents h)))]
                                             (accept ws-added h-updated nbest alts explained
                                                     delta comparison cycle)))
                                         ws-conflicts (:hyps hyp)))]
            (update-in ws-composite [:accrej :acc] conj (:id hyp))))))

;; TODO
(defn update-kb
  [workspace]
  (if-not (:UpdateKB params) workspace
          workspace))

(defn get-explaining-hypotheses
  "Ask problem domain to get explainers."
  [workspace time-now]
  (do (swap! calls-to-hypothesize assoc (:simulation params)
             (inc (get @calls-to-hypothesize (:simulation params) 0)))
      (doall ((:hypothesize-fn (:abduction @problem))
              (unexplained workspace) (accepted workspace) (hypotheses workspace) time-now))))

(defn update-hypotheses
  "Put explainers from problem domain into workspace."
  [workspace cycle time-now]
  (do
    (log "Updating hypotheses")
    (let [hyps (get-explaining-hypotheses workspace time-now)
          ;; possibly drop some % of lowest-scoring hyps
          hyps-ablated (drop (int (* (count hyps) (/ (:AblatePct params) 100.0)))
                             (my-shuffle hyps))]
      (reduce #(add %1 %2 cycle) workspace hyps-ablated))))

(defn reject-minscore
  [workspace cycle]
  (reduce (fn [ws h] (reject ws h :minscore cycle))
          workspace (filter (fn [h] (and (undecided? workspace h)
                                         (not= :observation (:type h))
                                         (< (:apriori h) (double (/ (:MinScore params) 100.0)))
                                         (not (prevented-rejection? workspace h :minscore))))
                            (:all (hypotheses workspace)))))

(defn contrast-sets
  [workspace unexp]
  (filter (comp not-empty :expl)
          (for [h unexp] {:hyp h :expl (filter #(undecided? workspace %)
                                               (explainers workspace h))})))

(defn contrast-set-delta
  [{:keys [hyp expl]}]
  (let [sorted-expl (reverse (sort-by :apriori expl))
        normalized-aprioris (let [aprioris (map :apriori sorted-expl)
                                  s (reduce + aprioris)]
                              (if (= 0.0 (double s)) aprioris
                                  (map #(/ % s) aprioris)))
        delta (if (second normalized-aprioris)
                (- (first normalized-aprioris)
                   (second normalized-aprioris))
                1.0)]
    {:hyp hyp :expl sorted-expl :delta delta :top-apriori (:apriori (first sorted-expl))}))

(defn best-contrast-set
  [workspace c-sets]
  (if (= (:ContrastPreference params) "arbitrary")
    (assoc (sort-by (comp :id :hyp) (my-rand-nth c-sets)) :delta 1.0)
    (let [delta-c-sets (map contrast-set-delta c-sets)
          best-delta (apply max (map :delta delta-c-sets))
          best-delta-c-sets (filter #(= best-delta (:delta %)) delta-c-sets)
          best-top-apriori (apply max (map :top-apriori best-delta-c-sets))
          best-top-apriori-c-sets (filter #(= best-top-apriori (:top-apriori %)) best-delta-c-sets)
          best-c-set (last (sort-by (fn [{:keys [expl]}] (count (explains workspace (first expl)))) best-top-apriori-c-sets))]
      best-c-set)))

(defn find-best
  [workspace]
  (let [unexp (unexplained workspace)
        c-sets (contrast-sets workspace unexp)
        {:keys [hyp expl delta]} (when (not-empty c-sets) (best-contrast-set workspace c-sets))]
    (when hyp
      (let [best (first expl)
            nbest (second expl)]
        (log "best:" best "nbest:" nbest "delta:" delta)
        (when (or (= 0 (:Threshold params))
                  (>= delta (+ 0.001 (/ (:Threshold params) 100.0))))
          {:best best :nbest nbest :delta delta
           :explained hyp :alts (rest expl)
           :contrast-sets c-sets})))))

(defn record-best-in-accgraph
  [workspace hyp contrast-sets]
  (let [newly-explained (filter #(unexplained? workspace %)
                                (set/intersection (:unexplained workspace)
                                                  (set (map :id (explains workspace hyp)))))
        deltas (into {} (for [explained newly-explained]
                          (let [expl (:expl (first (filter #(= explained (:id (:hyp %))) contrast-sets)))]
                            [explained
                             (cond (= [hyp] expl) 1.0
                                   (= hyp (first expl)) (- (:apriori hyp)
                                                           (:apriori (second expl)))
                                   (not-empty expl)
                                   (- (:apriori hyp) (:apriori (first expl)))
                                   :else 1.0)])))
        ag-expl (reduce (fn [ag hypid]
                          (-> ag
                              (add-edges [(:id hyp) hypid])
                              (add-attr hypid :score (:apriori (lookup-hyp workspace hypid)))
                              (add-attr hypid :label (format "%d / %.2f" hypid (:apriori (lookup-hyp workspace hypid))))
                              (add-attr (:id hyp) hypid :delta (get deltas hypid))
                              (add-attr (:id hyp) hypid :label (format "%.2f" (get deltas hypid)))))
                        (-> (:accgraph workspace)
                            (add-attr (:id hyp) :score (:apriori hyp))
                            (add-attr (:id hyp) :label (format "%d / %.2f" (:id hyp) (:apriori hyp))))
                        (keys deltas))]
    (assoc workspace :accgraph ag-expl)))

(defn explain
  [workspace cycle]
  (let [ws (assoc workspace :accrej {})
        ws-minscore (reject-minscore ws cycle)]
    (log "Unexplained:" (str/join ", " (sort (map :id (unexplained ws-minscore)))))
    (let [{:keys [best nbest alts explained delta comparison contrast-sets] :as b} (find-best ws-minscore)]
      (if-not best
        (do (log "No best.") ws-minscore)
        (do (log "Best is" (:id best) (:apriori best))
            (-> ws-minscore
                (update-in [:accrej] merge b)
                ;; record-best-in-accgraph must occur before accept in
                ;; order to figure out what is newly explained (TODO:
                ;; fix this)
                (record-best-in-accgraph best contrast-sets)
                (accept best nbest alts explained delta comparison cycle)))))))

(defn add-sensor-hyps
  "Ask problem domain to make sensor hyps; then put them into workspace."
  [workspace time-prev time-now sensors cycle anomalies]
  (do
    (log "Adding sensor hyps")
    (swap! calls-to-observe assoc (:simulation params)
           (inc (get @calls-to-observe (:simulation params) 0)))
    (let [hs ((:make-sensor-hyps-fn (:abduction @problem))
              sensors time-prev time-now
              (accepted workspace) (hypotheses workspace) anomalies)
          ws-added (reduce #(add %1 %2 cycle) workspace hs)
          ws-accepted (reduce #(accept %1 %2 nil [] [] 0.0 {} cycle)
                              ws-added
                              (filter (fn [h] (undecided? ws-added h))
                                      (map #(lookup-hyp ws-added (:id %)) hs)))]
      (if (:ClearAccGraphSensors params)
        (assoc ws-accepted :accgraph (digraph))
        ws-accepted))))

(defn add-kb
  [workspace hyps]
  (reduce (fn [ws h]
            (let [ws-added (add ws h 0)]
              (if (= :kb (:type h))
                (-> ws-added
                    (update-in [:hypgraph] add-attr (:id h) :accepted? true)
                    (update-in [:accepted] conj (:id h)))
                ws-added)))
          workspace hyps))

(defn init-kb
  [workspace training]
  (add-kb workspace ((:generate-kb-fn (:abduction @problem)) training)))

(defn init-workspace
  []
  (assoc empty-workspace
    :oracle-types
    (set (map keyword (str/split (:Oracle params) #",")))
    :meta-oracle-types
    (set (map keyword (str/split (:MetaOracle params) #",")))))

(defn get-unexp-pct
  [workspace]
  (let [unexp (unexplained workspace)
        acc (accepted workspace)]
    (if (empty? unexp) 0.0
        (/ (double (count unexp))
           (double (count (:all acc)))))))

(defn get-noexp-pct
  [workspace]
  (let [noexp (no-explainers workspace)
        acc (accepted workspace)]
    (if (empty? noexp) 0.0
        (/ (double (count noexp))
           (double (count (:all acc)))))))

(defn calc-doubt-from-accgraph-recursive
  [accgraph hypid]
  (let [this-score (attr accgraph hypid :score)
        children (neighbors accgraph hypid)]
    (cond (= "mult" (:DoubtAccGraphBranch params))
          (if (empty? children) this-score
              (let [ds (for [child-id children]
                         (cond (= "delta" (:DoubtAccGraphBranchMult params))
                               (* (attr accgraph hypid child-id :delta)
                                  (calc-doubt-from-accgraph-recursive accgraph child-id))
                               (= "score" (:DoubtAccGraphBranchMult params))
                               (* this-score
                                  (calc-doubt-from-accgraph-recursive accgraph child-id))
                               (= "score-delta-prod" (:DoubtAccGraphBranchMult params))
                               (* this-score (attr accgraph hypid child-id :delta)
                                  (calc-doubt-from-accgraph-recursive accgraph child-id))
                               (= "min-score-delta" (:DoubtAccGraphBranchMult params))
                               (* (min this-score (attr accgraph hypid child-id :delta))
                                  (calc-doubt-from-accgraph-recursive accgraph child-id))
                               (= "max-score-delta" (:DoubtAccGraphBranchMult params))
                               (* (max this-score (attr accgraph hypid child-id :delta))
                                  (calc-doubt-from-accgraph-recursive accgraph child-id))))]
                (cond (= "min" (:DoubtAccGraphAgg params))
                      (apply min ds)
                      (= "max" (:DoubtAccGraphAgg params))
                      (apply max ds)
                      (= "avg" (:DoubtAccGraphAgg params))
                      (avg ds))))
          (= "inv-delta" (:DoubtAccGraphBranch params))
          (if (empty? children) this-score
              (let [ds (for [child-id (filter #(not-empty (neighbors accgraph %)) children)]
                         (calc-doubt-from-accgraph-recursive accgraph child-id))
                    this-deltas (map #(attr accgraph hypid % :delta) children)
                    this-inv-deltas (map #(- 1.0 (max 0.0 %)) this-deltas)
                    this-doubt (min this-score (- 1.0 (reduce * this-inv-deltas)))]
                (avg (conj ds this-doubt)))))))

(defn view-accgraph
  [workspace]
  (javax.swing.SwingUtilities/invokeLater
   (fn [] (view (:accgraph workspace)))))

(defn calc-doubt-from-accgraph
  [workspace]
  (let [ag (:accgraph workspace)
        tops (filter #(empty? (incoming ag %)) (nodes ag))]
    (when-not (empty? tops)
      (let [ds (map #(calc-doubt-from-accgraph-recursive ag %) tops)
            noexp (no-explainers workspace)
            ds-noexp (concat (mapcat (fn [h] (repeat (:DoubtNoExp params) (:apriori h))) noexp) ds)]
        (- 1.0 (cond (= "min" (:DoubtAccGraphAgg params))
                     (apply min ds-noexp)
                     (= "max" (:DoubtAccGraphAgg params))
                     (apply max ds-noexp)
                     (= "avg" (:DoubtAccGraphAgg params))
                     (avg ds-noexp)))))))

(defn calc-doubt
  [workspace]
  (let [best (when (and (:best (:accrej workspace))
                        (or (not (:DoubtIgnoreEssentials params))
                            (:nbest (:accrej workspace)))
                        (not ((:ignore-doubt-types (:abduction @problem))
                              (:type (:best (:accrej workspace)))))
                        (:delta (:accrej workspace)))
               (:best (:accrej workspace)))
        score (when best (:apriori best))
        delta (when best (:delta (:accrej workspace)))
        expl (when best (cond (= "none" (:DoubtExplainedModifier params))
                              []
                              (= "each" (:DoubtExplainedModifier params))
                              (map :apriori (explains workspace best))
                              (= "explained" (:DoubtExplainedModifier params))
                              [(:apriori (:explained (:accrej workspace)))]
                              (= "max" (:DoubtExplainedModifier params))
                              [(apply max (map :apriori (explains workspace best)))]
                              (= "min" (:DoubtExplainedModifier params))
                              [(apply min (map :apriori (explains workspace best)))]
                              (= "avg" (:DoubtExplainedModifier params))
                              [(avg (map :apriori (explains workspace best)))]))]
    (when-let [d (cond (= "score-delta-prod" (:DoubtMeasure params))
                       (when (and score delta) (- 1.0 (* score delta)))
                       (= "score-delta-pow" (:DoubtMeasure params))
                       (when (and score delta) (- 1.0 (Math/pow score (- 1.0 delta))))
                       (= "max-score-delta" (:DoubtMeasure params))
                       (when (and score delta) (- 1.0 (max score delta)))
                       (= "min-score-delta" (:DoubtMeasure params))
                       (when (and score delta) (- 1.0 (min score delta)))
                       (= "accgraph" (:DoubtMeasure params))
                       (calc-doubt-from-accgraph workspace)
                       (= "weighted-score-delta" (:DoubtMeasure params))
                       (when (and score delta)
                         (+ (* (:DoubtScoreWeight params) (- 1.0 score))
                            (* (- 1.0 (:DoubtScoreWeight params)) (- 1.0 delta))))
                       (= "avg-score-delta-expl" (:DoubtMeasure params))
                       (when (and score delta)
                         (avg (concat [(- 1.0 score) (- 1.0 delta)] expl)))
                       (= "score" (:DoubtMeasure params))
                       (when score (- 1.0 score))
                       (= "delta" (:DoubtMeasure params))
                       (when delta (- 1.0 delta))
                       :else
                       (when delta (- 1.0 delta)))]
      (cond (= "square" (:DoubtModifier params))
            (* d d)
            (= "cube" (:DoubtModifier params))
            (* d d d)
            (= "sqrt" (:DoubtModifier params))
            (Math/sqrt d)
            (= "log" (:DoubtModifier params))
            (Math/log (+ 1.0 d))
            :else ;; "none"
            d))))

