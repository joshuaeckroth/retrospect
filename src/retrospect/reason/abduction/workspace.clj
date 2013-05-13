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
  (:use [retrospect.profile :only [prof profile]])
  (:use [retrospect.logging])
  (:use [retrospect.evaluate :only [avg]])
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

(defn accepted-rivals
  [workspace h]
  (prof
   :accepted-rivals
   (let [hypid (if (integer? h) h (:id h))]
     (attr (:hypgraph workspace) hypid :accepted-rivals))))

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
          (needs-explainer? workspace hypid)
          (not-any? (fn [h2] (accepted? workspace h2)) (explainers workspace hypid))))))

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
              (every? (fn [expl] (and (rejected? workspace expl)
                                     (not= :ignoring (rejection-reason workspace expl))))
                      expls))))))

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
                      (fn [hyps] (doall (my-shuffle hyps)))
                      ;; even if not "arbitrary", perform a shuffle so
                      ;; the rgen (random generator) stays in sync
                      ;; with runs that don't use arbitrary hyp-pref
                      (fn [hyps] (doall (my-shuffle hyps))
                        (sort (partial compare-hyps workspace unexp) hyps)))
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
                           identity)]
     ;; perform a shuffle first so the rgen stays in sync across
     ;; arbitrary and non-arbitrary runs
     (expl-sorter (my-shuffle (map #(update-in % [:expl] hyp-sorter) explainers))))))

(defn conflicts?
  [h1 h2]
  (prof
   :conflicts?
   (if-let [c? (get-in @cache [:conflicts (:simulation params) (:id h1) (:id h2)])]
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
  (prof
   :find-conflicts
   (filter #(conflicts? hyp %) (vals (:hyp-ids workspace)))))

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
  ;; includes this hyp
  (let [g (transpose (:hypgraph workspace))]
    (bf-traverse (fn [hypid] (concat (neighbors g hypid)
                                    (map :id (find-conflicts workspace (lookup-hyp workspace hypid)))))
                 (:id hyp))))

(defn related-hyps?
  [workspace hyp1 hyp2]
  (not-empty (set/intersection (set (related-hyps workspace hyp1))
                    (set (related-hyps workspace hyp2)))))

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
             (update-in [:unexplained] set/union (set (attr (:hypgraph workspace) hypid :accepted-newly-explained)))
             (add-to-hyp-log hypid "Undecided")))
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
                     (update-in [:unexplained] disj (:id hyp))
                     (update-in [:accrej :rej] conj (:id hyp)))
               ;; is this hyp a member of any composites? if so, those
               ;; composites need to be rejected as well
               composites (filter (fn [h] (and (:composite? h) (some (fn [hc] (= hyp hc)) (:hyps h))))
                             (:all (hypotheses workspace)))
               ws-comp (reduce (fn [ws h] (reject ws h reason-tag cycle)) ws composites)]
           (add-to-hyp-log ws-comp hyp (format "%s rejected at cycle %d with reason %s" (str hyp) cycle (str reason-tag))))))))

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
   (let [ ;; sometimes, an added hyp explains stuff not already added;
         ;; this can happen when a problem case is added during abductive metareasoning;
         ;; we just ignore these non-existing explained hyps
         explains (filter identity (map #(get (:hyp-contents workspace) %) (:explains hyp)))]
     (-> workspace
        (assoc-in [:hyp-ids (:id hyp)] hyp)
        (assoc-in [:hyp-contents (:contents hyp)] (:id hyp))
        (update-in [:hypgraph] add-nodes (:id hyp))
        (update-in [:hypgraph] #(apply add-edges % (for [e explains] [(:id hyp) e])))
        (update-in [:hypotheses (:type hyp)] conj (:id hyp))
        (update-in [:hypotheses :all] conj (:id hyp))
        (?> (:needs-explainer? hyp) update-in [:unexplained] conj (:id hyp))))))

(defn add-existing-hyp-rejected
  [workspace hyp]
  ;; if it was rejected due to :minscore and it would not again be
  ;; rejected for the same reason, unreject it
  (prof
   :add-existing-hyp-rejected
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
                                 :apriori (max (:apriori hyp) (:apriori prior-hyp))
                                 :data (:data hyp)
                                 :contents (assoc (:data hyp) :type (:type hyp) :subtype (:subtype hyp)))
             ;; ensure hyp is updated with new :explains, :apriori, etc.
             ws (add-helper workspace prior-hyp-updated)]
         (if (rejected? ws prior-hyp)
           ;; possibly unreject it
           (add-existing-hyp-rejected ws prior-hyp-updated)
           ws))))))

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
     (let [ws (if-let [prior-hyp-id (get (:hyp-contents workspace) (:contents hyp-c))]
                (-> workspace
                   (add-to-hyp-log prior-hyp-id (format "%s Added as updated hyp %s at cycle %d" hyp-c prior-hyp-id cycle))
                   (add-existing-hyp-updated hyp-c prior-hyp-id cycle))
                (-> workspace
                   (add-to-hyp-log hyp-c (format "Added at cycle %d" cycle))
                   (add-helper hyp-c)))]
       (if (and (not= :observation (:type hyp-c))
                (not (rejected? ws hyp-c))
                (conflicts-with-accepted? ws hyp-c))
         (do (log (str "...yet it conflicts with an already accepted hyp, "
                       "so immediately rejecting."))
             (reject ws hyp-c :conflict cycle))
         ws)))))

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
             (update-in ws-composite [:accrej :acc] conj (:id hyp)))))))

;; TODO
(defn update-kb
  [workspace]
  (prof
   :update-kb
   (if-not (:UpdateKB params) workspace
           workspace)))

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

(defn reject-minscore
  [workspace cycle]
  (reduce (fn [ws h] (reject ws h :minscore cycle))
     workspace (filter (fn [h] (and (not= :observation (:type h))
                              (undecided? workspace h)
                              (<= (:apriori h) (double (/ (:MinScore params) 100.0)))
                              (not (prevented-rejection? workspace h :minscore))))
                  (:all (hypotheses workspace)))))

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
              :comparison {} :delta 1.0 :normalized-aprioris [1.0]
              :contrast-sets expls-sorted})
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
                :comparison comparison
                :contrast-sets expls-sorted}))))))))

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
  (prof
   :explain
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
                (accept best nbest alts explained delta comparison cycle))))))))

(defn add-observation
  [workspace hyp cycle]
  (prof
   :add-observation
   (let [ws-added (add workspace hyp cycle)]
     (if (rejected? ws-added hyp) ws-added
         (accept ws-added hyp nil [] [] 0.0 {} cycle)))))

(defn add-sensor-hyps
  "Ask problem domain to make sensor hyps; then put them into workspace."
  [workspace time-prev time-now sensors cycle]
  (prof
   :add-sensor-hyps
   (do
     (log "Adding sensor hyps")
     (let [hs ((:make-sensor-hyps-fn (:abduction @problem))
               sensors time-prev time-now
               (accepted workspace) (hypotheses workspace))
           ws (reduce #(add-observation %1 %2 cycle) workspace hs)]
       (if (:ClearAccGraphSensors params)
         (assoc ws :accgraph (digraph))
         ws)))))

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

(defn calc-doubt-from-accgraph-recursive
  [accgraph hypid]
  (let [this-score (attr accgraph hypid :score)
        children (neighbors accgraph hypid)]
    (if (empty? children) 1.0
      (let [ds (for [child-id children]
                 (cond (= "delta" (:DoubtAccGraphMult params))
                       (* (attr accgraph hypid child-id :delta)
                          (calc-doubt-from-accgraph-recursive accgraph child-id))
                       (= "score" (:DoubtAccGraphMult params))
                       (* this-score
                          (calc-doubt-from-accgraph-recursive accgraph child-id))
                       (= "score-delta-prod" (:DoubtAccGraphMult params))
                       (* this-score (attr accgraph hypid child-id :delta)
                          (calc-doubt-from-accgraph-recursive accgraph child-id))
                       (= "min-score-delta" (:DoubtAccGraphMult params))
                       (* (min this-score (attr accgraph hypid child-id :delta))
                          (calc-doubt-from-accgraph-recursive accgraph child-id))
                       (= "max-score-delta" (:DoubtAccGraphMult params))
                       (* (max this-score (attr accgraph hypid child-id :delta))
                          (calc-doubt-from-accgraph-recursive accgraph child-id))))]
        (cond (= "min" (:DoubtAccGraphAgg params))
              (apply min ds)
              (= "max" (:DoubtAccGraphAgg params))
              (apply max ds)
              (= "avg" (:DoubtAccGraphAgg params))
              (avg ds))))))

(defn view-accgraph
  [workspace]
  (javax.swing.SwingUtilities/invokeLater
   (fn [] (view (:accgraph workspace)))) )

(defn calc-doubt-from-accgraph
  [workspace]
  (let [ag (:accgraph workspace)
        tops (filter #(empty? (incoming ag %)) (nodes ag))]
    (when-not (empty? tops)
      (let [ds (map #(calc-doubt-from-accgraph-recursive ag %) tops)]
        (- 1.0 (cond (= "min" (:DoubtAccGraphAgg params))
                     (apply min ds)
                     (= "max" (:DoubtAccGraphAgg params))
                     (apply max ds)
                     (= "avg" (:DoubtAccGraphAgg params))
                     (avg ds)))))))

(defn calc-doubt
  [workspace]
  (prof
   :calc-doubt
   (let [best (when (and (:best (:accrej workspace))
                         (or (not (:DoubtIgnoreEssentials params))
                             (:nbest (:accrej workspace)))
                         (not ((:ignore-doubt-types (:abduction @problem))
                               (:type (:best (:accrej workspace))))))
                (:best (:accrej workspace)))
         score (when best (:apriori best))
         delta (when (and best (:delta (:accrej workspace)))
                 (:delta (:accrej workspace)))]
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
                        (when (and score delta) (+ (* (:DoubtScoreWeight params) (- 1.0 score))
                                                   (* (- 1.0 (:DoubtScoreWeight params)) (- 1.0 delta))))
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

