(ns retrospect.reason.abduction.workspace
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:use [plumbing.core])
  (:use [loom.graph :only
         [digraph nodes incoming neighbors weight
          add-nodes add-edges remove-nodes edges has-edge? transpose]])
  (:use [loom.alg-generic :only [bf-traverse]])
  (:use [loom.attr :only [add-attr remove-attr attr]])
  (:use [retrospect.profile :only [prof profile]])
  (:use [retrospect.logging])
  (:use [geppetto.random])
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
  ;; contents not provided; make them
  ([prefix type subtype apriori needs-explainer? conflicts?-fn
    explains short-str desc data]
   (new-hyp prefix type subtype apriori needs-explainer? conflicts?-fn
            explains short-str desc data
            (assoc data :type type :subtype subtype)))
  ;; contents provided; allows the hyp generator to decide what constitutes
  ;; an identical hyp (note that :data will be copied from new hyp
  ;; when an identical hyp is detected)
  ([prefix type subtype apriori needs-explainer? conflicts?-fn
    explains short-str desc data contents]
     (let [id (inc last-id)]
       (set-last-id id)
       (assoc (merge (Hypothesis.
                      id (format "%s%d" prefix id)
                      type subtype apriori needs-explainer? conflicts?-fn
                      explains short-str desc data)
                     data)
         :contents contents))))

(defn new-composite
  [prefix type subtype apriori explains short-str desc data hyps]
  (let [hyp (new-hyp prefix type subtype apriori false nil
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
   :hyp-contents {}})

(defn lookup-hyp
  [workspace id]
  (prof
   :lookup-hyp
   (get-in workspace [:hyp-ids id])))

(defn accepted?
  [workspace h]
  (prof
   :accepted?
   (let [hypid (if (integer? h) h (:id h))]
     ((:accepted workspace) hypid))))

(defn accepted
  [workspace]
  (prof
   :accepted
   (let [all (doall (map #(lookup-hyp workspace %) (:accepted workspace)))]
     (merge (group-by :type all) {:all all}))))

(defn hypotheses
  [workspace]
  (prof
   :hypotheses
   (let [g (:hypgraph workspace)
         ;; intentionally lazy (not sure if it helps, since using group by below)
         all (map #(lookup-hyp workspace %) (nodes g))]
     (merge (group-by :type all) {:all all}))))

;; TODO
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
  [workspace h]
  (prof
   :accepted-cycle
   (let [hypid (if (integer? h) h (:id h))]
     (attr (:hypgraph workspace) hypid :accepted-cycle))))

(defn accepted-explained
  [workspace h]
  (prof
   :accepted-explained
   (let [hypid (if (integer? h) h (:id h))]
     (attr (:hypgraph workspace) hypid :accepted-explained))))

;; TODO
(defn accepted-rivals
  [workspace hyp]
  (prof
   :accepted-rivals
   (get-in workspace [:accepted-rivals (:id hyp)])))

(defn rejected?
  [workspace h]
  (prof
   :rejected?
   (let [hypid (if (integer? h) h (:id h))]
     ((:rejected workspace) hypid))))

(defn rejected
  [workspace]
  (prof
   :rejected
   (let [all (doall (map #(lookup-hyp workspace %) (:rejected workspace)))]
     (merge (group-by :type all) {:all all}))))

;; TODO
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
  [workspace h]
  (prof
   :rejected-cycle
   (let [hypid (if (integer? h) h (:id h))]
     (attr (:hypgraph workspace) hypid :rejected-cycle))))

(defn rejection-reason
  [workspace h]
  (prof
   :rejected-reason
   (let [hypid (if (integer? h) h (:id h))]
     (attr (:hypgraph workspace) hypid :rejection-reason))))

(defn prevent-rejection
  [workspace h rej-reason]
  (prof
   :prevent-rejection
   (let [hypid (if (integer? h) h (:id h))]
     (update-in workspace [:hypgraph]
                add-attr hypid :prevent-rejection rej-reason))))

(defn prevented-rejection?
  [workspace h rej-reason]
  (prof
   :prevented-rejection?
   (let [hypid (if (integer? h) h (:id h))]
     (= rej-reason (attr (:hypgraph workspace) hypid :prevent-rejection)))))

(defn undecided?
  [workspace h]
  (prof
   :undecided?
   (and (not (accepted? workspace h))
        (not (rejected? workspace h)))))

(defn undecided
  [workspace]
  (prof
   :undecided
   (let [g (:hypgraph workspace)]
     (doall (map #(lookup-hyp workspace %)
               (set/difference (nodes g) (:accepted workspace) (:rejected workspace)))))))

(defn explains?
  "Does h1 explain h2?"
  [workspace h1 h2]
  (prof
   :explains?
   (let [hypid1 (if (integer? h1) h1 (:id h1))
         hypid2 (if (integer? h2) h2 (:id h2))]
     (has-edge? (:hypgraph workspace) hypid1 hypid2))))

(defn explains
  [workspace h]
  (prof
   :explains
   (let [hypid (if (integer? h) h (:id h))]
     (doall (map #(lookup-hyp workspace %) (neighbors (:hypgraph workspace) hypid))))))

(defn explainers
  [workspace h]
  (prof
   :explainers
   (let [hypid (if (integer? h) h (:id h))]
     (doall (map #(lookup-hyp workspace %) (incoming (:hypgraph workspace) hypid))))))

(defn needs-explainer?
  [workspace h]
  (prof
   :needs-explainer?
   (if (integer? h)
     (:needs-explainer? (lookup-hyp workspace h))
     (:needs-explainer? h))))

(defn unexplained?
  [workspace h]
  (prof
   :unexplained?
   (let [hypid (if (integer? h) h (:id h))]
     (and ((:unexplained workspace) hypid)
          (accepted? workspace hypid)
          (needs-explainer? workspace hypid)))))

(defn unexplained
  [workspace]
  (prof
   :unexplained
   (doall (map #(lookup-hyp workspace %)
             (filter #(unexplained? workspace %) (:unexplained workspace))))))

(defn no-explainers?
  [workspace h]
  (prof
   :no-explainers?
   (let [hypid (if (integer? h) h (:id h))
         expls (explainers workspace hypid)]
     (and (unexplained? workspace hypid)
          (or (empty? expls)
              (every? (fn [expl] (rejected? workspace expl)) expls))))))

(defn no-explainers
  [workspace]
  (prof
   :no-explainers
   (doall (map #(lookup-hyp workspace %) (filter #(no-explainers? workspace %)
                                          (:unexplained workspace))))))

(defn hyp-log
  [workspace h]
  (prof
   :hyp-log
   (let [hypid (if (integer? h) h (:id h))]
     (attr (:hypgraph workspace) hypid :log))))

(defn hyp-better-than?
  [workspace unexp hyp1 hyp2]
  (prof
   :hyp-better-than?
   (let [score (> (double (- (:apriori hyp1) (:apriori hyp2))) 0.001)
         expl (> (count (filter #(explains? workspace hyp1 %) unexp))
                 (count (filter #(explains? workspace hyp2 %) unexp)))]
     {:score score :expl expl})))

(defn compare-hyps
  "Since we are using probabilities, smaller value = less
   confidence. We want most confident first. With equal confidences,
   we look for higher explanatory power (explains more). If all that
   fails, comparison is done by the :id's (to keep it deterministic)."
  [workspace unexp hyp1 hyp2]
  (prof
   :compare-hyps
   (let [comp1 (hyp-better-than? workspace unexp hyp1 hyp2)
         comp2 (hyp-better-than? workspace unexp hyp2 hyp1)
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
                                                    s (double (reduce + aprioris))]
                                                (if (= 0.0 s) aprioris
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
  [workspace unexp explainers]
  (prof
   :sort-explainers
   (let [hyp-sorter (if (= (:HypPreference params) "arbitrary")
                      #(my-shuffle %)
                      #(sort (partial compare-hyps workspace unexp) %))
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
     (expl-sorter (map #(update-in % [:expl] hyp-sorter) explainers)))))

(defn conflicts?
  [h1 h2]
  (prof
   :conflicts?
   (if-let [c? (get-in @cache [:conflicts (:simulation params) (:id h1) (:id h2)])]
     c?
     (let [c? (cond (not= (:type h1) (:type h2))
                    false
                    (:composite? h2)
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

(defn find-conflicts-all
  [workspace hyp]
  (prof
   :find-conflicts-all
   (filter #(conflicts? hyp %) (vals (:hyp-ids workspace)))))

;; TODO
(defn find-conflicts-active
  [workspace hyp]
  (prof
   :find-conflicts-active
   (filter #(conflicts? hyp %)
      (map #(lookup-hyp workspace %)
         (set (apply concat (vals (:sorted-explainers workspace))))))))

(defn conflicts-with-accepted?
  [workspace hyp]
  (prof
   :conflicts-with-accepted?
   (some (fn [hyp2] (conflicts? hyp hyp2))
      (filter #(= (:type hyp) (:type %))
         (map #(lookup-hyp workspace %) (filter (fn [hypid] (accepted? workspace hypid))
                                         (nodes (:hypgraph workspace))))))))

(defn related-hyps
  [workspace hyp]
  (let [g (transpose (:hypgraph workspace))]
    (bf-traverse (fn [hypid] (concat (neighbors g hypid)
                                    (map :id (find-conflicts-all workspace (lookup-hyp workspace hypid)))))
                 (:id hyp))))

(defn undecide
  [workspace hyp]
  (prof
   :undecide
   ;; note related-hyps includes hyp itself
   (let [rel-hyps (related-hyps workspace hyp)]
     (log "Undeciding" hyp "and related hyps" rel-hyps)
     (reduce (fn [ws hypid]
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
             (update-in [:unexplained] set/union
                        (attr (:hypgraph workspace) hypid
                              :accepted-newly-explained))))
        workspace rel-hyps))))

(defn unreject
  [workspace hyp]
  (prof
   :unreject
   (-> workspace
      (update-in [:hypgraph] remove-attr (:id hyp) :rejected?)
      (update-in [:hypgraph] remove-attr (:id hyp) :rejected-cycle)
      (update-in [:hypgraph] remove-attr (:id hyp) :rejection-reason)
      (update-in [:rejected] disj (:id hyp)))))

;; forward declaration so (reject) can refer to (add) and vice versa
(declare add)

(defn reject
  "Each 'rejected' hypothesis will be removed from the list of hyps
   that need to be explained, and any other hyps a rejected hyp
   explains will be marked as no longer potentially explained by the
   rejected hyp."
  [workspace hyp reason-tag cycle]
  (prof
   :reject
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
                     (update-in [:unexplained] disj (:id hyp)))
               ;; is this hyp a member of any composites? if so, those
               ;; composites need to be rejected as well
               composites (filter (fn [h] (and (:composite? h) (some (fn [hc] (= hyp hc)) (:hyps h))))
                             (:all (hypotheses workspace)))
               ws-comp (reduce (fn [ws h] (reject ws h reason-tag cycle)) ws composites)]
           (if @batch ws-comp
               (assoc-in ws-comp [:hyp-log (:id hyp)]
                         (format "Rejected at cycle %d with reason %s"
                            cycle (str reason-tag)))))))))

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

(defn add-helper
  [workspace hyp]
  (prof
   :add-helper
   (let [;; sometimes, an added hyp explains stuff not already added;
         ;; this can happen when a problem case is added during abductive metareasoning;
         ;; we just ignore these non-existing explained hyps
         explains (filter identity (map #(get (:hyp-contents workspace) %) (:explains hyp)))]
     (-> workspace
        (assoc-in [:hyp-ids (:id hyp)] hyp)
        (assoc-in [:hyp-contents (:contents hyp)] (:id hyp))
        (update-in [:hypgraph] add-nodes (:id hyp))
        (update-in [:hypgraph] #(apply add-edges % (for [e explains] [(:id hyp) e])))
        (?> (:needs-explainer? hyp) update-in [:unexplained] conj (:id hyp))
        (update-in [:hypotheses (:type hyp)] conj (:id hyp))
        (update-in [:hypotheses :all] conj (:id hyp))))))

(defn add-existing-hyp-rejected
  [workspace prior-hyp prior-hyp-updated]
  ;; if it was rejected due to :minscore and it would not again be
  ;; rejected for the same reason, unreject it
  (prof
   :add-existing-hyp-rejected
   (if (and (= :minscore (rejection-reason workspace prior-hyp))
            (>= (:apriori prior-hyp-updated)
               (double (/ (:MinScore params) 100.0))))
     (do (log "...yet was rejected due to :minscore previously\n"
              "...but now satisfies minscore, so unrejecting.")
         (unreject workspace prior-hyp-updated))
     (do (log "...yet was rejected due to" (rejection-reason workspace prior-hyp)
              "so leaving as is (not adding).")
         workspace))))

(defn add-existing-hyp-updated
  "hyp already present; update explains in case it changed,
   and whether it needs explanation or not"
  [workspace hyp prior-hyp-id cycle]
  (prof
   :add-existing-hyp-updated
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
                                 :apriori (:apriori hyp)
                                 :data (:data hyp))]
         (if (rejected? workspace prior-hyp)
           (add-existing-hyp-rejected workspace prior-hyp prior-hyp-updated)
           (add-helper workspace prior-hyp-updated)))))))

(defn add
  [workspace hyp cycle]
  (prof
   :add
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
     (if-let [prior-hyp-id (get (:hyp-contents workspace) (:contents hyp-c))]
       (add-existing-hyp-updated workspace hyp-c prior-hyp-id cycle)
       ;; add it, then check for conflicts and minscore issues; if no issues, keep it
       (let [ws (add-helper workspace hyp-c)]
         (cond (conflicts-with-accepted? ws hyp-c)
               (do (log (str "...yet it conflicts with an already accepted hyp, "
                             "so immediately rejecting."))
                   (reject ws hyp-c :conflict cycle))
               (and (<= (:apriori hyp-c) (double (/ (:MinScore params) 100.0)))
                    (not (prevented-rejection? ws hyp-c :minscore)))
               (do (log (str "...yet it does not meet MinScore requirement, "
                             "so immediately rejecting."))
                   (reject ws hyp-c :minscore cycle))
               :else ws))))))

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
           (let [newly-explained (set/intersection (:unexplained workspace)
                                        (set (map :id (explains workspace hyp))))
                 ws-acc (-> workspace
                           (update-in [:hypgraph] add-attr (:id hyp)
                                      :accepted? true)
                           (update-in [:hypgraph] add-attr (:id hyp)
                                      :accepted-cycle cycle)
                           (update-in [:hypgraph] add-attr (:id hyp)
                                      :accepted-explained explained)
                           (update-in [:hypgraph] add-attr (:id hyp)
                                      :accepted-newly-explained newly-explained)
                           (update-in [:accepted] conj (:id hyp))
                           (update-in [:unexplained] set/difference newly-explained))
                 ws-hyplog (if @batch ws-acc
                               (update-in ws-acc [:hypgraph]
                                          add-attr (:id hyp) :log
                                          (format (str "Accepted at cycle %d to explain %s with delta %.2f "
                                                  "(essential? %s; next-best: %s); "
                                                  "comparison: %s")
                                             cycle explained delta (nil? nbest) nbest
                                             comparison)))
                 conflicts (prof :accept-conflicts (find-conflicts-all ws-hyplog hyp))
                 ws-conflicts (prof :accept-reject
                                    (reduce (fn [ws h] (reject ws h :conflict cycle))
                                       ws-hyplog conflicts))
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
                                   ws-conflicts (:hyps hyp))
                                ws-conflicts)]
             (update-in ws-composite [:accrej :acc] conj (:id hyp)))))))

(defn add-observation
  [workspace hyp cycle]
  (prof
   :add-observation
   (let [ws-added (add workspace hyp cycle)]
     (if (rejected? ws-added hyp) ws-added
         (accept ws-added hyp nil [] [] 0.0 {} cycle)))))

(defn get-unexp-pct
  [workspace]
  (prof
   :get-unexp-pct
   (let [unexp (unexplained workspace)
         acc (accepted workspace)]
     (if (empty? unexp) 0.0
         (/ (double (count unexp))
            (double (count (:all acc))))))))

(defn get-noexp-pct
  [workspace]
  (prof
   :get-noexp-pct
   (let [noexp (no-explainers workspace)
         acc (accepted workspace)]
     (if (empty? noexp) 0.0
         (/ (double (count noexp))
            (double (count (:all acc))))))))

(defn calc-doubt
  [workspace]
  (prof
   :calc-doubt
   (let [score (when (and (:best (:accrej workspace))
                          (not ((:ignore-doubt-types (:abduction @problem))
                                (:type (:best (:accrej workspace))))))
                 (:apriori (:best (:accrej workspace))))
         delta (when (and (:delta (:accrej workspace))
                          (not ((:ignore-doubt-types (:abduction @problem))
                                (:type (:best (:accrej workspace))))))
                 (:delta (:accrej workspace)))]
     (when-let [d (cond (= "score" (:DoubtMeasure params))
                        (when score (- 1.0 score))
                        (= "delta" (:DoubtMeasure params))
                        (when delta (- 1.0 delta))
                        (= "score-delta-prod" (:DoubtMeasure params))
                        (when (and score delta) (* score delta))
                        (= "score-delta-avg" (:DoubtMeasure params))
                        (when (and score delta) (- 1.0 (/ (+ score delta) 2.0)))
                        (= "score-delta-pow" (:DoubtMeasure params))
                        (when (and score delta) (Math/pow (- 1.0 delta) score))
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
             d)))))

(defn contrast-sets
  [workspace unexp]
  (let [expls (doall (filter #(not-empty (:expl %))
                        (for [h unexp] {:hyp h :expl (filter #(undecided? workspace %)
                                                        (explainers workspace h))})))]
    (sort-explainers workspace unexp expls)))

(defn find-best
  [workspace]
  (prof
   :find-best
   (let [unexp (unexplained workspace)
         expls-sorted (contrast-sets workspace unexp)]
     (when (not-empty expls-sorted)
       (let [essential-expl (first (filter #(= 1 (count (:expl %))) expls-sorted))]
         (if essential-expl
           (let [explained (:hyp essential-expl)
                 best (first (:expl essential-expl))]
             {:best best :nbest nil :explained explained :alts []
              :comparison {} :delta 1.0 :normalized-aprioris [1.0]})
           ;; otherwise, choose highest-delta non-essential
           (let [hyp-expl (first expls-sorted)
                 explained (:hyp hyp-expl)
                 choices (:expl hyp-expl)
                 best (first choices)
                 nbest (second choices)
                 normalized-aprioris (let [aprioris (map :apriori choices)
                                           s (reduce + aprioris)]
                                       (if (= 0.0 (double s)) aprioris
                                           (map #(/ % s) aprioris)))
                 delta (- (first normalized-aprioris) (second normalized-aprioris))
                 comparison (hyp-better-than? workspace unexp best nbest)]
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
                :explained explained :alts (rest choices)
                :comparison comparison}))))))))

;; TODO
(defn update-kb
  [workspace]
  (prof
   :update-kb
   (if-not (:UpdateKB params) workspace
           workspace)))

;; TODO
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

(defn get-explaining-hypotheses
  "Ask problem domain to get explainers."
  [workspace time-now]
  (prof
   :get-explaining-hypotheses
   (doall ((:hypothesize-fn (:abduction @problem))
           (unexplained workspace) (accepted workspace) (hypotheses workspace) time-now))))

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
   (let [ws (assoc workspace :accrej {})]
     (log "Unexplained:" (map :id (unexplained ws)))
     (let [{:keys [best nbest alts explained delta comparison] :as b} (find-best ws)]
       (if-not best
         (do (log "No best.") ws)
         (do (log "Best is" (:id best) (:apriori best))
             (-> ws (update-in [:accrej] merge b)
                (accept best nbest alts explained delta comparison cycle))))))))

(defn add-sensor-hyps
  "Ask problem domain to make sensor hyps; then put them into workspace."
  [workspace time-prev time-now sensors cycle]
  (prof
   :add-sensor-hyps
   (do
     (log "Adding sensor hyps")
     (let [hs ((:make-sensor-hyps-fn (:abduction @problem))
               sensors time-prev time-now
               (accepted workspace) (hypotheses workspace))]
       (reduce #(add-observation %1 %2 cycle) workspace hs)))))

(defn add-kb
  [workspace hyps]
  (prof
   :add-kb
   (reduce (fn [ws h]
        (let [ws-added (add ws h 0)]
          (if (= :kb (:type h))
            (-> ws-added
               (update-in [:hypgraph] add-attr (:id h) :accepted? true)
               (update-in [:accepted] conj (:id h)))
            ws-added)))
      workspace hyps)))

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
