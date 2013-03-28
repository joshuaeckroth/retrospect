(ns retrospect.reason.abduction.workspace
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
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
  (toString [self] (format "%s(%s)/%.2f" name short-str apriori))
  Comparable
  (compareTo [self other] (compare (hash self) (hash other))))

(defn new-hyp
  [prefix type subtype apriori needs-explainer? conflicts?-fn
   explains short-str desc data]
  (let [id (inc last-id)]
    (set-last-id id)
    (assoc (merge (Hypothesis.
                   id (format "%s%d" prefix id)
                   type subtype apriori needs-explainer? conflicts?-fn
                   explains short-str desc data)
                  data)
      :contents (assoc data :type type :subtype subtype))))

(defn new-composite
  [prefix type subtype apriori explains short-str desc data hyps]
  (let [hyp (new-hyp prefix type subtype apriori false nil
                     explains short-str desc data)]
    (assoc hyp :composite? true :hyps hyps)))

(defmethod print-method Hypothesis
  [h w]
  (print-simple (format "%s(%s)/%.2f" (:name h) (:short-str h) (:apriori h)) w))

(def empty-workspace
  {:graph (digraph)
   :oracle nil
   :meta-oracle nil
   ;; what was accepted, rejected, merged with the 'best' map
   :accrej {}
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
   ;; a map of type => seq, with additional key :all
   :hypotheses {}
   ;; :data + :type map keys => hyp-id values (for dup searching)
   :hyp-contents {}
   ;; a map of type => set, with additional key :all
   :accepted {:all #{}}
   ;; a map of hyp-id => integer
   :accepted-cycle {}
   ;; a map of type => set, with additional key :all
   :rejected {:all #{}}
   ;; a map of hyp-id => integer
   :rejected-cycle {}
   ;; a map of hyp-id => reason-tag (e.g., :conflict)
   :rejection-reasons {}
   ;; a set of [hyp-contents rej-reason] specifying which hyps should not be rejected for which reasons
   :prevent-rejections #{}
   ;; a map of hyp-id => hyp what was explained when it was accepted
   :accepted-explained {}
   ;; a map of hyp-id => seq of hyps of rival explainers when it was accepted
   :accepted-rivals {}
   ;; a map of hypid => set of hypids
   :sorted-explainers {}
   ;; a seq of hypids
   :sorted-explainers-explained '()
   ;; do the sorted explainers need to be udpated?
   :dirty false})

(defn lookup-hyp
  [workspace id]
  (prof
   :lookup-hyp
   (get-in workspace [:hyp-ids id])))

(defn accepted?
  [workspace hyp]
  (prof
   :accepted?
   ((get-in workspace [:accepted :all] #{}) (:id hyp))))

(defn accepted-before?
  "Was hyp accepted at or before hyp2 was accepted/rejected?"
  [workspace hyp hyp2]
  (prof
   :accepted-before?
   (if-let [cycle (or (get-in workspace [:accepted-cycle (:id hyp2)])
                      (get-in workspace [:rejected-cycle (:id hyp2)]))]
     (and (accepted? workspace hyp)
          (<= (get-in workspace [:accepted-cycle (:id hyp)]) cycle))
     false)))

(defn accepted-cycle
  [workspace hyp]
  (prof
   :accepted-cycle
   (get-in workspace [:accepted-cycle (:id hyp)])))

(defn accepted-explained
  [workspace hyp]
  (prof
   :accepted-explained
   (get-in workspace [:accepted-explained (:id hyp)])))

(defn accepted-rivals
  [workspace hyp]
  (prof
   :accepted-rivals
   (get-in workspace [:accepted-rivals (:id hyp)])))

(defn rejected?
  [workspace hyp]
  (prof
   :rejected?
   ((get-in workspace [:rejected :all] #{}) (:id hyp))))

(defn rejected-before?
  "Was hyp rejected at or before hyp2 was accepted/rejected?"
  [workspace hyp hyp2]
  (prof
   :rejected-before?
   (if-let [cycle (or (get-in workspace [:accepted-cycle (:id hyp2)])
                      (get-in workspace [:rejected-cycle (:id hyp2)]))]
     (and (rejected? workspace hyp)
          (<= (get-in workspace [:rejected-cycle (:id hyp)]) cycle))
     false)))

(defn rejected-cycle
  [workspace hyp]
  (prof
   :rejected-cycle
   (get-in workspace [:rejected-cycle (:id hyp)])))

(defn rejection-reason
  [workspace hyp]
  (prof
   :rejected-reason
   (get-in workspace [:rejection-reasons (:id hyp)])))

(defn prevent-rejection
  [workspace hyps rej-reason]
  (prof
   :prevent-rejection
   (update-in workspace [:prevent-rejections]
              set/union (set (map (fn [h] [(:contents h) rej-reason]) hyps)))))

(defn prevented-rejection?
  [workspace hyp rej-reason]
  (prof
   :prevented-rejection?
   ((:prevent-rejections workspace) [(:contents hyp) rej-reason])))

(defn unexplained?
  [workspace hyp]
  (prof
   :unexplained?
   (#{(:id hyp)} (:sorted-explainers-explained workspace))))

(defn hyp-log
  [workspace hyp]
  (prof
   :hyp-log
   (get (:hyp-log workspace) (:id hyp))))

(defn explains
  [workspace hyp]
  (prof
   :explains
   (doall (filter identity (map #(lookup-hyp workspace %)
                         (get (:explains workspace) (:id hyp)))))))

(defn explainers
  [workspace hyp]
  (prof
   :explainers
   (doall (filter identity (map #(lookup-hyp workspace %)
                         (get (:explainers workspace) (:id hyp)))))))

(defn hyp-better-than?
  [workspace hyp1 hyp2]
  (prof
   :hyp-better-than?
   (let [score (> (double (- (:apriori hyp1) (:apriori hyp2))) 0.001)
         expl (> (count (filter (fn [hyp-id] (some #(= % hyp-id)
                                          (:sorted-explainers-explained workspace)))
                           (get (:explains workspace) (:id hyp1))))
                 (count (filter (fn [hyp-id] (some #(= % hyp-id)
                                          (:sorted-explainers-explained workspace)))
                           (get (:explains workspace) (:id hyp2)))))]
     {:score score :expl expl})))

(defn compare-hyps
  "Since we are using probabilities, smaller value = less
   confidence. We want most confident first. With equal confidences,
   we look for higher explanatory power (explains more). If all that
   fails, comparison is done by the :id's (to keep it deterministic)."
  [workspace hyp1 hyp2]
  (prof
   :compare-hyps
   (let [comp1 (hyp-better-than? workspace hyp1 hyp2)
         comp2 (hyp-better-than? workspace hyp2 hyp1)
         pref (:HypPreference params)
         id-compare (compare (:id hyp1) (:id hyp2))]
     (cond (= "score" pref) (cond (:score comp1) -1
                                  (:score comp2) 1
                                  :else id-compare)
           (= "expl" pref) (cond (:expl comp1) -1
                                 (:expl comp2) 1
                                 :else id-compare)
           (= "score,expl" pref) (cond (:score comp1) -1
                                       (:score comp2) 1
                                       (:expl comp1) -1
                                       (:expl comp2) 1
                                       :else id-compare)
           (= "expl,score" pref) (cond (:expl comp1) -1
                                       (:expl comp2) 1
                                       (:score comp1) -1
                                       (:score comp2) 1
                                       :else id-compare)
           :else id-compare))))

(defn compare-by-delta
  [workspace expl1 expl2]
  (prof
   :compare-by-delta
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
     (if (= 0 (compare expl1-delta expl2-delta))
       (compare (:id (first expl1)) (:id (first expl2)))
       (- (compare expl1-delta expl2-delta))))))

(defn sort-explainers
  [workspace explainers]
  (prof
   :sort-explainers
   (let [hyp-sorter (if (= (:HypPreference params) "arbitrary")
                      #(my-shuffle %)
                      #(sort (partial compare-hyps workspace) %))
         apriori-sorter (fn [{hyp1 :hyp expl1 :expl} {hyp2 :hyp expl2 :expl}]
                          (- (compare
                              (:apriori (first expl1))
                              (:apriori (first expl2)))))
         apriori-delta-sorter (fn [{hyp1 :hyp expl1 :expl} {hyp2 :hyp expl2 :expl}]
                                (let [hyp-apriori (- (compare
                                                      (:apriori (first expl1))
                                                      (:apriori (first expl2))))
                                      d (compare-by-delta workspace expl1 expl2)]
                                  (if (= 0 hyp-apriori) d hyp-apriori)))
         delta-apriori-sorter (fn [{hyp1 :hyp expl1 :expl} {hyp2 :hyp expl2 :expl}]
                                (let [hyp-apriori (- (compare
                                                      (:apriori (first expl1))
                                                      (:apriori (first expl2))))
                                      d (compare-by-delta workspace expl1 expl2)]
                                  (if (= 0 d) hyp-apriori d)))
         delta-sorter (fn [{hyp1 :hyp expl1 :expl} {hyp2 :hyp expl2 :expl}]
                        (compare-by-delta workspace expl1 expl2))
         expl-sorter (cond (= (:ContrastPreference params) "score")
                           (fn [hs] (sort apriori-sorter hs))
                           (= (:ContrastPreference params) "score,delta")
                           (fn [hs] (sort apriori-delta-sorter hs))
                           (= (:ContrastPreference params) "delta,score")
                           (fn [hs] (sort delta-apriori-sorter hs))
                           (= (:ContrastPreference params) "delta")
                           (fn [hs] (sort delta-sorter hs))
                           (= (:ContrastPreference params) "arbitrary")
                           #(my-shuffle %))]
     (expl-sorter (doall (map #(update-in % [:expl] hyp-sorter) explainers))))))

(defn update-sorted-explainers
  [workspace]
  (prof
   :update-sorted-explainers
   (do
     (log "Updating sorted explainers.")
     (let [expls (set
                  (map (fn [hypid]
                       {:hyp (lookup-hyp workspace hypid)
                        :expl (doall
                               (map #(lookup-hyp workspace %)
                                  (get-in workspace [:sorted-explainers hypid])))})
                     (:sorted-explainers-explained workspace)))
           expls-sorted (sort-explainers workspace expls)]
       (reduce (fn [ws {h :hyp expl :expl}]
            (assoc-in ws [:sorted-explainers (:id h)] (doall (map :id expl))))
          (assoc workspace :sorted-explainers-explained
                 (doall (map (comp :id :hyp) expls-sorted))
                 :dirty false)
          expls-sorted)))))

(defn conflicts?
  [h1 h2]
  (prof
   :conflicts?
   (cond (:composite? h2)
         (some (fn [h] ((:conflicts?-fn h) h1 h)) (:hyps h2))
         (:composite? h1)
         (some (fn [h] ((:conflicts?-fn h) h h2)) (:hyps h1))
         (or (nil? (:conflicts?-fn h1)) (nil? (:conflicts?-fn h2)))
         false
         :else
         ((:conflicts?-fn h1) h1 h2))))

(defn find-conflicts-all
  [workspace hyp]
  (prof
   :find-conflicts-all
   (doall (filter #(conflicts? hyp %) (vals (:hyp-ids workspace))))))

(defn find-conflicts
  [workspace hyp]
  (prof
   :find-conflicts
   (doall (filter #(conflicts? hyp %)
             (map #(lookup-hyp workspace %)
                (set (apply concat (vals (:sorted-explainers workspace)))))))))

(defn dissoc-needing-explanation
  [workspace hyps]
  (prof
   :dissoc-needing-explanation
   (reduce (fn [ws h]
        (log "Dissociating" h "as needing explanation.")
        (-> ws (assoc :sorted-explainers-explained
                (filter #(not= (:id h) %)
                   (:sorted-explainers-explained ws)))
           (update-in [:sorted-explainers] dissoc (:id h))))
      workspace hyps)))

(defn assoc-needing-explanation
  [workspace hyp]
  (prof
   :assoc-needing-explanation
   (let [all-expls (get-in workspace [:explainers (:id hyp)])
         avail-expls (filter #(not ((get-in workspace [:rejected :all]) %))
                        all-expls)]
     (log "Associating" hyp "as now needing explanation.")
     (-> workspace
        (update-in [:sorted-explainers-explained] conj (:id hyp))
        (assoc-in [:explainers (:id hyp)] all-expls)
        (assoc-in [:sorted-explainers (:id hyp)] avail-expls)
        (assoc :dirty true)))))

(defn assoc-explainer
  [workspace hyp]
  (prof
   :assoc-explainer
   (reduce (fn [ws h]
        (log "Associating" hyp "as explainer of" h)
        (-> ws
           ;; add to explainers cache
           (update-in [:explainers (:id h)] conjs (:id hyp))
           ;; add to active explainers
           (update-in [:sorted-explainers (:id h)] conjs (:id hyp))))
      (assoc workspace :dirty true) (explains workspace hyp))))

(defn forget-explainer
  [workspace hyp]
  (prof
   :forget-explainer
   (reduce (fn [ws h]
        (log (format "Forgetting that %s explains %s" (str hyp) (str h)))
        (-> ws
           (update-in [:explainers (:id h)] disj (:id hyp))
           (assoc-in [:sorted-explainers (:id h)]
                     (filter #(not= (:id hyp) %)
                        (get-in ws [:sorted-explainers (:id h)])))))
      (assoc workspace :dirty true) (explains workspace hyp))))

(defn dissoc-explainer
  "Given some hypothesis, dissociate it as a potential explainer for
   anything."
  [workspace hyp]
  (prof
   :dissoc-explainer
   (reduce (fn [ws h]
        (log (format "Dissociating %s as an explainer of %s" (str hyp) (str h)))
        (assoc-in ws [:sorted-explainers (:id h)]
                  (filter #(not= (:id hyp) %)
                     (get-in ws [:sorted-explainers (:id h)]))))
      workspace (explains workspace hyp))))

;; forward declaration so (reject-many) can refer to (add) and vice versa
(declare add)

(defn reject-many
  "Each 'rejected' hypothesis will be removed from the list of hyps
   that need to be explained, and any other hyps a rejected hyp
   explains will be marked as no longer potentially explained by the
   rejected hyp."
  [workspace hyps reason-tag cycle]
  (prof
   :reject-many
   (reduce
    (fn [ws hyp]
      (log "Rejecting" hyp "with reason" reason-tag)
      (let [ws-added (if (nil? (get (:hyp-contents workspace) (:contents hyp)))
                       ;; hyp not known yet (rejecting preemptively); add first
                       (add ws hyp cycle)
                       ws)
            ws2 (-> (dissoc-in ws-added [:sorted-explainers (:id hyp)])
                   (update-in [:accepted :all] disj (:id hyp))
                   (update-in [:accepted (:type hyp)] disj (:id hyp))
                   (update-in [:accrej :rej] conjs (:id hyp))
                   (update-in [:rejected (:type hyp)] conjs (:id hyp))
                   (assoc-in [:rejected-cycle (:id hyp)] cycle)
                   (update-in [:rejected :all] conjs (:id hyp))
                   (assoc-in [:rejection-reasons (:id hyp)] reason-tag)
                   (dissoc-needing-explanation [hyp])
                   (dissoc-explainer hyp))]
        (if @batch ws2
            (assoc-in ws2 [:hyp-log (:id hyp)]
                      (format "Rejected at cycle %d with reason %s"
                              cycle (str reason-tag))))))
    workspace (filter #(not (rejected? workspace %)) hyps))))

(defn record-if-needs-explanation
  [workspace hyp]
  (prof
   :record-if-needs-explanation
   (if (:needs-explainer? hyp)
     (do
       (log "Recording" hyp "as needing explanation.")
       (update-in workspace [:needs-explanation] conj (:id hyp)))
     workspace)))

(defn update-hyp-apriori
  [workspace hyp]
  (prof
   :update-hyp-apriori
   (cond
    ;; check oracle
    ((:oracle-types workspace) (:type hyp))
    (if ((:oracle workspace) hyp)
      (assoc hyp :apriori 1.0)
      (assoc hyp :apriori 0.0))
    ;; check meta-oracle
    ((:meta-oracle-types workspace) (:type hyp))
    (if ((:meta-oracle workspace) hyp)
      (assoc hyp :apriori 1.0)
      (assoc hyp :apriori 0.0))
    :else
    (if (not (:UseScores params))
      (assoc hyp :apriori 1.0)
      (if (= 100 (:ScoreLevels params)) hyp
          (let [levels (range 0.0 1.01 (/ 1.0 (double (dec (:ScoreLevels params)))))
                apriori-new (first (sort-by #(Math/abs (- (:apriori hyp) %)) levels))]
            (assoc hyp :apriori apriori-new)))))))

(defn add
  [workspace hyp cycle]
  (prof
   :add
   (let [hyp-explains (set (map #(get (:hyp-contents workspace) %) (:explains hyp)))
         ;; update a composite hyp so that pre-existing
         ;; components are used instead of duplicate components
         ;; in the new composite
         hyp-c (if (:composite? hyp)
                 (assoc hyp :hyps (map (fn [h] (if-let [h-id (get (:hyp-contents workspace)
                                                               (:contents h))]
                                              (lookup-hyp workspace h-id) h))
                                     (:hyps hyp)))
                 hyp)]
     (log "Adding" hyp-c "which explains" hyp-explains)
     (if-let [prior-hyp-id (get (:hyp-contents workspace) (:contents hyp-c))]
       ;; hyp already present; update explains in case it changed,
       ;; and whether it needs explanation or not
       (let [prior-hyp (lookup-hyp workspace prior-hyp-id)
             new-hyp-apriori (:apriori (update-hyp-apriori workspace hyp-c))
             prior-hyp-updated (assoc prior-hyp
                                 :needs-explainer? (:needs-explainer? hyp-c)
                                 :explains (set/union (set (:explains prior-hyp))
                                                  (set (:explains hyp-c)))
                                 :apriori (min (:apriori prior-hyp)
                                               new-hyp-apriori))
             new-explains (set (map #(get (:hyp-contents workspace) %)
                                  (:explains prior-hyp-updated)))]
         (log hyp-c "is already in the workspace as" prior-hyp-updated
              ", so merging what it explains to obtain:" new-explains)
         (let [ws (-> workspace
                     (forget-explainer prior-hyp)
                     (assoc-in [:hyp-ids prior-hyp-id] prior-hyp-updated)
                     (assoc-in [:explains prior-hyp-id] new-explains)
                     (assoc-explainer prior-hyp-updated)
                     (record-if-needs-explanation prior-hyp-updated))]
           (if ((get-in ws [:rejected :all]) prior-hyp-id)
             ;; if it was rejected due to :minscore and it
             ;; would not again be rejected for the same reason,
             ;; unreject it
             (if (and (= :minscore (rejection-reason ws prior-hyp))
                      (>= (:apriori prior-hyp-updated) (double (/ (:MinScore params) 100.0))))
               (do (log "...yet was rejected due to :minscore previously\n"
                        "...but now satisfies minscore, so unrejecting.")
                   (-> ws (update-in [:rejected :all] disj prior-hyp-id)
                      (update-in [:rejected (:type prior-hyp)] disj prior-hyp-id)
                      (update-in [:accrej :rej] disj prior-hyp-id)
                      (dissoc-in [:rejection-reasons prior-hyp-id])))
               (do (log "...yet was rejected due to" (rejection-reason ws prior-hyp)
                        "so leaving as is (not adding).")
                   (-> ws (dissoc-in [:sorted-explainers prior-hyp-id])
                      (dissoc-explainer prior-hyp-updated))))
             ;; it was already accepted
             (if ((get-in ws [:accepted :all]) prior-hyp-id)
               (do (log "...yet was already accepted.")
                   (-> ws
                      (dissoc-needing-explanation (explains ws prior-hyp-updated))
                      (dissoc-in [:sorted-explainers prior-hyp-id])
                      (dissoc-explainer prior-hyp-updated)))
               ;; it may conflict with an accepted hyp
               (if (and (:conflicts?-fn hyp-c)
                        (some (fn [hyp2] ((:conflicts?-fn hyp-c) hyp-c hyp2))
                           (map #(lookup-hyp ws %) (:all (:accepted ws)))))
                 (do (log "...yet it conflicts with an already accepted hyp, so immediately rejecting.")
                     (reject-many ws [prior-hyp] :conflict cycle))
                 ;; otherwise, just leave it with its explainers
                 ;; updated, etc. (from above)
                 ws)))))
       ;; otherwise, check if conflicts, if not add it
       (let [ws (let [hyp-apriori (update-hyp-apriori workspace hyp-c)]
                  (-> workspace
                     (assoc-in [:hyp-ids (:id hyp-apriori)] hyp-apriori)
                     (assoc-in [:hyp-contents (:contents hyp-apriori)] (:id hyp-apriori))
                     (assoc-in [:explains (:id hyp-apriori)] hyp-explains)
                     (record-if-needs-explanation hyp-apriori)
                     (assoc-explainer hyp-apriori)
                     (update-in [:hypotheses (:type hyp-apriori)] conj (:id hyp-apriori))
                     (update-in [:hypotheses :all] conj (:id hyp-apriori))))]
         (if (and (:conflicts?-fn hyp-c)
                  (some (fn [hyp2] ((:conflicts?-fn hyp) hyp-c hyp2))
                     (map #(lookup-hyp workspace %) (:all (:accepted workspace)))))
           (do (log "...yet it conflicts with an already accepted hyp, so rejecting.")
               (reject-many ws [hyp-c] :conflict cycle))
           ws))))))

(defn accept
  [workspace hyp nbest alts explained delta comparison cycle]
  (prof
   :accept
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
           (let [ws-acc (prof :accept-update
                              (-> workspace
                                 (update-in [:accepted (:type hyp)] conjs (:id hyp))
                                 (update-in [:accepted :all] conjs (:id hyp))
                                 (assoc-in [:accepted-explained (:id hyp)] explained)
                                 (assoc-in [:accepted-rivals (:id hyp)] alts)
                                 (assoc-in [:accepted-cycle (:id hyp)] cycle)))
                 ws-hyplog (if @batch ws-acc
                               (assoc-in ws-acc [:hyp-log (:id hyp)]
                                         (format (str "Accepted at cycle %d to explain %s with delta %.2f "
                                                 "(essential? %s; next-best: %s); "
                                                 "comparison: %s")
                                            cycle explained delta (nil? nbest) nbest
                                            comparison)))
                 ws-expl (dissoc-needing-explanation ws-hyplog (explains workspace hyp))
                 conflicts (prof :accept-conflicts
                                 (when (:conflicts?-fn hyp)
                                   (find-conflicts-all ws-expl hyp)))
                 ws-conflicts (if conflicts
                                (prof :accept-reject-many
                                      (reject-many ws-expl conflicts :conflict cycle))
                                ws-expl)
                 ws-needs-exp (if (or (not ((:needs-explanation ws-conflicts) (:id hyp)))
                                      (some #(accepted? ws-conflicts %) (explainers ws-conflicts hyp)))
                                ws-conflicts
                                (assoc-needing-explanation ws-conflicts hyp))
                 ws-composite (if (:composite? hyp)
                                (reduce (fn [ws h]
                                     (let [ws-added (add ws h cycle)
                                           ;; when added, hyp id may have
                                           ;; changed; find the hyp again
                                           h-updated (lookup-hyp ws-added
                                                                 (get (:hyp-contents ws-added)
                                                                      (:contents h)))]
                                       (accept ws-added h-updated nbest alts explained
                                               delta comparison cycle)))
                                   ws-needs-exp (:hyps hyp))
                                ws-needs-exp)]
             (update-in ws-composite [:accrej :acc] conj (:id hyp)))))))

(defn add-observation
  [workspace hyp cycle]
  (prof
   :add-observation
   (let [ws-added (add workspace hyp cycle)]
     (if (rejected? ws-added hyp) ws-added
         (accept ws-added hyp nil [] [] 0.0 {} cycle)))))

(defn get-unexplained
  [workspace]
  (prof
   :get-unexplained
   (:sorted-explainers-explained workspace)))

(defn get-unexp-pct
  [workspace]
  (prof
   :get-unexp-pct
   (let [unexp (get-unexplained workspace)]
     (if (empty? unexp) 0.0
         (/ (double (count unexp))
            (double (count (filter (:needs-explanation workspace)
                              (:all (:accepted workspace))))))))))

(defn get-no-explainers
  [workspace]
  (prof
   :get-no-explainers
   (filter #(empty? (get (:sorted-explainers workspace) %))
      (:sorted-explainers-explained workspace))))

(defn get-noexp-pct
  [workspace]
  (prof
   :get-noexp-pct
   (let [noexp (get-no-explainers workspace)]
     (if (empty? noexp) 0.0
         (/ (double (count noexp))
            (double (count (filter (:needs-explanation workspace)
                              (:all (:accepted workspace))))))))))

(defn calc-doubt
  [workspace]
  (prof
   :calc-doubt
   (if (= "score" (:DoubtMeasure params))
     (when (and (:best (:accrej workspace))
                (not ((:ignore-doubt-types (:abduction @problem))
                      (:type (:best (:accrej workspace))))))
       (- 1.0 (:apriori (:best (:accrej workspace)))))
     ;; "delta"
     (when (and (:delta (:accrej workspace))
                (not ((:ignore-doubt-types (:abduction @problem))
                      (:type (:best (:accrej workspace))))))
       (- 1.0 (:delta (:accrej workspace)))))))

(defn calc-coverage
  [workspace]
  (prof
   :calc-coverage
   (let [acc-needs-exp (set (filter (:needs-explanation workspace)
                               (:all (:accepted workspace))))]
     (if (empty? acc-needs-exp) 1.0
         (let [accessible (mapcat (fn [hyp-id] (pre-traverse (:graph workspace) hyp-id))
                                  acc-needs-exp)]
           (/ (double (count (set/intersection acc-needs-exp (set accessible))))
              (double (count acc-needs-exp))))))))

(defn find-unaccepted
  [workspace]
  (prof
   :find-unaccepted
   (set/difference
    (set (:all (:hypotheses workspace)))
    (set (:all (:accepted workspace))))))

(defn find-best
  [workspace]
  (prof
   :find-best
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
             {:best best :nbest nil :explained essential :alts []
              :comparison {} :delta 1.0 :normalized-aprioris [1.0]})
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
  (prof
   :update-kb
   (if-not (:UpdateKB params) workspace
           (let [new-kb-hyps ((:update-kb-fn (:abduction @problem))
                              (:accepted workspace)
                              (get-unexplained workspace)
                              (:hypotheses workspace)
                              (partial lookup-hyp workspace))]
             (-> (reduce (fn [ws h] (assoc-in ws [:hyp-ids (:id h)] h))
                   workspace new-kb-hyps)
                (assoc-in [:accepted :kb] (set (map :id new-kb-hyps)))
                (update-in [:accepted :all] set/union (set (map :id new-kb-hyps))))))))

(defn update-graph
  "Need to run this before evaluation in order to calculate coverage."
  [workspace]
  (prof
   :update-graph
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
                          g-expl (:all (:hypotheses workspace))))
         g-accepted (reduce
                     (fn [g h]
                       (-> g (add-attr h :fontcolor "green")
                           (add-attr h :color "green")))
                     g-conflicts (:all (:accepted workspace)))]
     (assoc workspace :graph g-accepted))))

(defn clean-up-workspace
  "Find & reject pending rejections due to conflicts or too-low apriori values."
  [workspace cycle]
  (prof
   :cleanup-workspace
   (loop [ws workspace
          hyps (map #(lookup-hyp ws %) (:all (:hypotheses ws)))]
     (if (empty? hyps) ws
         (let [hyp (first hyps)]
           (log "Checking whether" hyp "should be cleaned up...")
           (cond (rejected? ws hyp)
                 (do (log "...already rejected. Moving on.")
                     (recur ws (rest hyps)))
                 
                 (accepted? ws hyp)
                 (do (log "...already accepted. Moving on.")
                     (recur ws (rest hyps)))

                 (and (not= :observation (:type hyp))
                      (< (:apriori hyp) (/ (double (:MinScore params)) 100.0))
                      (not (prevented-rejection? ws hyp :minscore)))
                 (do (log "...rejecting because score" (:apriori hyp)
                          "is lower than MinScore.")
                     (let [ws-next (reject-many ws [hyp] :minscore cycle)]
                       (recur ws-next (rest hyps))))
                 
                 (and (:composite? hyp)
                      (some (fn [h] (rejected? ws h)) (:hyps hyp)))
                 (do (log "...rejecting because this hyp is a composite of at least one rejected hyp.")
                     (let [first-rej-hyp (some (fn [h] (rejected? ws h)) (:hyps hyp))
                           ws-next (reject-many ws [hyp]
                                                (rejection-reason ws first-rej-hyp)
                                                cycle)]
                       (recur ws-next (rest hyps))))
                 
                 (some (fn [hyp2] (conflicts? hyp hyp2))
                       (map #(lookup-hyp ws %) (:all (:accepted ws))))
                 (do (log "...rejecting because of conflicts with:"
                          (str/join ", " (map str (filter (fn [hyp2] (conflicts? hyp hyp2))
                                                          (map #(lookup-hyp ws %) (:all (:accepted ws)))))))
                     (let [ws-next (reject-many ws [hyp] :conflict cycle)]
                       (recur ws-next (rest hyps))))
                 
                 :else
                 (do (log "...we'll keep this hyp.")
                     (recur ws (rest hyps)))))))))

(defn get-explaining-hypotheses
  "Ask problem domain to get explainers."
  [workspace time-now]
  (prof
   :get-explaining-hypotheses
   ((:hypothesize-fn (:abduction @problem))
    (map #(lookup-hyp workspace %) (:sorted-explainers-explained workspace))
    (:accepted workspace) (:hypotheses workspace)
    (partial lookup-hyp workspace) time-now)))

(defn update-hypotheses
  "Put explainers from problem domain into workspace."
  [workspace cycle time-now]
  (prof
   :update-hypotheses
   (do
     (log "Updating hypotheses")
     (let [hyps (get-explaining-hypotheses workspace time-now)]
       (reduce #(add %1 %2 cycle) workspace hyps)))))

(defn explain
  [workspace cycle time-now]
  (prof
   :explain
   (let [ws-explainers (if true #_(:dirty workspace)
                           (update-sorted-explainers workspace)
                           workspace)
         ws (clean-up-workspace (assoc ws-explainers :accrej {}) cycle)]
     (log "Explainers:" (:sorted-explainers ws)
          (format "[%s]" (str/join ", " (map str (:sorted-explainers-explained ws)))))
     (if (empty? (:sorted-explainers-explained ws))
       (do (log "No explainers.") ws)
       (let [{:keys [best nbest alts explained delta comparison] :as b}
             (find-best ws)]
         (if-not best
           (do (log "No best.") ws)
           (do (log "Best is" (:id best) (:apriori best))
               (-> ws (update-in [:accrej] merge b)
                  (accept best nbest alts explained delta comparison cycle)))))))))

(defn hyp-max-delta
  [ws hyp]
  (prof
   :hyp-max-delta
   ;; figure out the delta for this hyp; the delta is the greatest
   ;; delta we can find
   (let [contrast-sets (filter (fn [contrast-set]
                            (some #(= (:id hyp) %) contrast-set))
                          (vals (:sorted-explainers ws)))]
     (if (some #(= 1 (count %)) contrast-sets)
       ;; this hyp is an essential somewhere
       1.0
       (apply max (map (fn [contrast-set]
                       (if (= (:id hyp) (first contrast-set))
                         (- (:apriori hyp)
                            (:apriori (lookup-hyp ws (second contrast-set))))
                         (- (:apriori (lookup-hyp ws (first contrast-set)))
                            (:apriori hyp))))
                     contrast-sets))))))

(defn add-sensor-hyps
  "Ask problem domain to make sensor hyps; then put them into workspace."
  [workspace time-prev time-now sensors cycle]
  (prof
   :add-sensor-hyps
   (do
     (log "Adding sensor hyps")
     (let [hs ((:make-sensor-hyps-fn (:abduction @problem))
               sensors time-prev time-now
               (:accepted workspace) (:hypotheses workspace)
               (partial lookup-hyp workspace))]
       (reduce #(add-observation %1 %2 cycle) workspace hs)))))

(defn add-kb
  [workspace hyps]
  (prof
   :add-kb
   (reduce (fn [ws h]
        (let [ws-added (add ws h 0)]
          (if (= :kb (:type h))
            (-> ws-added
               (update-in [:accepted (:type h)] conjs (:id h))
               (update-in [:accepted :all] conjs (:id h)))
            ws-added)))
      workspace hyps)))

(defn remove-kb
  [workspace]
  (prof
   :remove-kb
   (let [kb-hyps (doall (map #(lookup-hyp workspace %)
                           (get-in workspace [:hypotheses :kb])))]
     (-> (reduce (fn [ws h] (dissoc-in ws [:hyp-contents (:contents h)]))
           workspace kb-hyps)
        (assoc-in [:accepted :kb] #{})
        (assoc-in [:hypotheses :kb] '())))))

(defn init-kb
  [workspace training]
  (prof
   :init-kb
   (add-kb workspace ((:generate-kb-fn (:abduction @problem)) training))))

(defn init-workspace
  []
  (prof
   :init-workspace
   (assoc empty-workspace
     :oracle-types
     (set (map keyword (str/split (:Oracle params) #",")))
     :meta-oracle-types
     (set (map keyword (str/split (:MetaOracle params) #","))))))
