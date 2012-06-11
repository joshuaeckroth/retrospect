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
  (:use [retrospect.state]))

(defrecord Hypothesis
    [id name type subtype needs-explainer? conflicts?-fn
     explains boosts short-str desc data]
  Object
  (toString [self] (format "%s(%s)" name short-str))
  Comparable
  (compareTo [self other] (compare (hash self) (hash other))))

(defn new-hyp
  [prefix type subtype needs-explainer? conflicts?-fn explains boosts short-str desc data]
  (prof :new-hyp
        (let [id (inc last-id)]
          (set-last-id id)
          (assoc
              (merge (Hypothesis.
                      id (format "%s%d" prefix id)
                      type subtype needs-explainer? conflicts?-fn explains
                      boosts short-str desc data)
                     data)
            :contents (assoc data :type type :subtype subtype)))))

(defn explains
  [hyp]
  (if (:TransitiveExplanation params)
    (filter #(not= hyp %) (tree-seq #(not-empty (:explains %)) :explains hyp))
    (:explains hyp)))

(defmethod print-method Hypothesis
  [o w]
  (print-simple (format "%s(%s)" (:name o) (:short-str o)) w))

(def empty-workspace
  {:graph (digraph)
   :oracle nil
   :cycle 0
   ;; hyp type => hyp subtype => score
   :scores {}
   ;; hyp type => hyp subtype => seq of history of scores
   :score-adjustments {}
   :log {:best [] :accrej {}}
   :hyp-log {}
   ;; all explainers, keyed by hyp-id, with vals as sets of hyp-ids; serves as a cache
   :explainers {}
   ;; a map of hyp-id => hyp
   :hyp-ids {}
   ;; a map of type => seq
   :hypotheses {}
   ;; a set of hypothesis :data + :type maps (for dup searching)
   :hyp-contents #{}
   ;; keyed by hyp-id
   :hyp-confidences {}
   ;; set of hyp-ids
   :forced #{}
   ;; a map of type => seq, with additional key :all
   :accepted {:all #{}}
   ;; a map of hypid => hyps
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
  (get (:hyp-log workspace) hyp))

(defn hyp-conf
  [workspace hyp]
  (prof :hyp-conf
        (if (:UseScores params)
          (get (:hyp-confidences workspace) (:id hyp))
          1.0)))

(defn find-no-explainers
  [workspace]
  (prof :find-no-explainers
        (map #(lookup-hyp workspace (first %))
           (filter (fn [[hypid expls]] (empty? expls)) (seq (:explainers workspace))))))

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
  (prof :normalize-confidences-groups
        (let [gs (reduce (fn [m h] (assoc m h (hyp-conf workspace h))) {} hyps)]
          (normalize-confidences gs))))

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
                              (assoc-in ws2 [:hyp-confidences (:id hyp)]
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
              explainers (- (compare
                             (count (get-in workspace [:explainers (:id hyp1)] []))
                             (count (get-in workspace [:explainers (:id hyp2)] []))))
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

(defn update-sorted-explainers
  [workspace]
  (prof :update-sorted-explainers
        (do
          (log "Updating sorted explainers.")
          (let [expls (filter (comp first :expl)
                         (map (fn [hypid]
                              {:hyp (lookup-hyp workspace hypid)
                               :expl (map #(lookup-hyp workspace %)
                                        (get-in workspace [:sorted-explainers hypid]))})
                            (:sorted-explainers-explained workspace)))
                expls-conf (if (= "none" (:ConfAdjustment params)) expls
                               (update-confidences workspace expls))
                expls-sorted (sort-explainers workspace expls-conf)]
            (reduce (fn [ws {h :hyp expl :expl}]
                 (assoc-in ws [:sorted-explainers (:id h)] (map :id expl)))
               (assoc workspace :sorted-explainers-explained
                      (map (comp :id :hyp) expls-sorted)
                      :dirty false)
               expls-sorted)))))

(defn boost
  [workspace hyp]
  (prof :boost
        (if (not (:ApplyBoosting params))
          workspace
          (let [new-conf (min 1.0 (+ 0.10 (hyp-conf workspace hyp)))]
            (-> workspace
               (assoc-in [:hyp-confidences (:id hyp)] new-conf)
               (update-in [:hyp-log hyp] conj
                          (format "Confidence boosted in cycle %d from %.2f to %.2f"
                             (:cycle workspace) (hyp-conf workspace hyp) new-conf)))))))

(defn find-conflicts-all
  [workspace hyp]
  (prof :find-conflicts-all
        (if (nil? (:conflicts hyp)) []
            (doall (filter #((:conflicts?-fn hyp) hyp %) (vals (:hyp-ids workspace)))))))

(defn find-conflicts
  [workspace hyp]
  (prof :find-conflicts
        (doall (filter #((:conflicts?-fn hyp) hyp (lookup-hyp workspace %))
                  (set (apply concat (vals (:sorted-explainers workspace))))))))

(defn dissoc-needing-explanation
  [workspace hyps]
  (prof :dissoc-needing-explanation
        (reduce (fn [ws h]
             (-> ws (assoc :sorted-explainers-explained
                     (filter #(not= (:id h) %)
                        (:sorted-explainers-explained ws)))
                (update-in [:sorted-explainers] dissoc (:id h))))
           workspace hyps)))

(defn assoc-needing-explanation
  [workspace hyp]
  (prof :assoc-needing-explanation
        (let [expls (get-in workspace [:explainers (:id hyp)])]
          (-> workspace
             (update-in [:sorted-explainers-explained] conj (:id hyp))
             (assoc-in [:explainers (:id hyp)] expls) ;; put the key in even if expls is empty
             (assoc :sorted-explainers expls)
             (assoc :dirty true)))))

(defn assoc-explainer
  [workspace hyp]
  (prof :assoc-explainer
        (reduce (fn [ws h] (-> ws
                        ;; add to explainers cache
                        (update-in [:explainers (:id h)] conj (:id hyp))
                        ;; add to active explainers
                        (update-in [:sorted-explainers (:id h)] conj (:id hyp))))
           (assoc workspace :dirty true) (explains hyp))))

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
             workspace (explains hyp)))))

(defn reject-many
  "Each 'rejected' hypothesis will be removed from the list of hyps
   that need to be explained, and any other hyps a rejected hyp
   explains will be marked as no longer potentially explained by the
   rejected hyp."
  [workspace hyps]
  (prof :reject-many
        (reduce
         (fn [ws hyp]
           (let [ws2 (-> (dissoc-in ws [:sorted-explainers (:id hyp)])
                        (dissoc-explainer hyp))]
             (if @batch ws2
                 (update-in ws2 [:hyp-log hyp] conj
                            (format "Rejected in cycle %d" (:cycle workspace))))))
         workspace hyps)))

(defn increase-score
  [workspace hyp]
  (let [prior (get-in workspace [:scores (:type hyp) (:subtype hyp)] [1 2])]
    (assoc-in workspace [:scores (:type hyp) (:subtype hyp)] (map inc prior))))

(defn decrease-score
  [workspace hyp]
  (let [prior (get-in workspace [:scores (:type hyp) (:subtype hyp)] [1 2])]
    (assoc-in workspace [:scores (:type hyp) (:subtype hyp)]
              [(first prior) (inc (second prior))])))

(defn lookup-score
  [workspace hyp]
  (prof :lookup-score
        (apply / (map double (get-in workspace [:scores (:type hyp) (:subtype hyp)] [1 2])))))

(defn add
  [workspace hyp]
  (prof :add
        (do
          (log "Adding" hyp)
          (if (prof :add-dup-search ((:hyp-contents workspace) (:contents hyp)))
            workspace
            (let [ws (prof :add-ws-update
                           (-> workspace
                              (update-in [:hyp-ids] assoc (:id hyp) hyp)
                              (update-in [:hyp-contents] conj (:contents hyp))
                              (assoc-explainer hyp)
                              (assoc-in
                               [:hyp-confidences (:id hyp)]
                               (cond ((:oracle-types workspace) (:type hyp))
                                     (if ((:oracle workspace) hyp) 1.0 0.0)
                                     :else
                                     (lookup-score workspace hyp)))
                              (update-in [:hypotheses (:type hyp)] conj (:id hyp))))]
              (if @batch ws
                  (prof :add-graph-update
                        (let [g-added (reduce (fn [g h]
                                           (-> g (add-nodes (:id h))
                                              (add-attr (:id h) :id (:id h))
                                              (add-attr (:id h) :label (:short-str h))))
                                         (:graph ws)
                                         (conj (explains hyp) hyp))
                              g-expl (reduce (fn [g e] (add-edges g [(:id hyp) (:id e)]))
                                        g-added (explains hyp))]
                          (assoc ws :graph g-expl)))))))))

(defn accept
  [workspace hyp explained delta essential?]
  (prof :accept
        (let [ws-acc (prof :accept-update
                           (-> workspace
                              (update-in [:accepted (:type hyp)] conj (:id hyp))
                              (update-in [:accepted :all] conj (:id hyp))))
              ws-hyplog (if @batch ws-acc
                            (update-in ws-acc [:hyp-log hyp] conj
                                       (format (str "Accepted in cycle %d "
                                               "to explain %s with delta %.2f"
                                               " (essential? %s)")
                                          (:cycle workspace)
                                          explained delta essential?)))
              ws-expl (dissoc-needing-explanation ws-hyplog (explains hyp))
              ws-expl2 (dissoc-explainer ws-expl hyp)
              conflicts (prof :accept-conflicts
                              (when (:conflicts hyp)
                                (find-conflicts ws-expl2 hyp)))
              ws-conflicts (if conflicts
                             (prof :accept-reject-many (reject-many ws-expl2 conflicts))
                             ws-expl2)
              ws-boosts (prof :accept-boosts (reduce boost ws-conflicts (:boosts hyp)))
              ws-needs-exp (prof :accept-needs-exp
                                 (assoc-needing-explanation ws-boosts hyp))]
          (prof :accept-final
                (update-in ws-boosts [:log :accrej (:cycle ws-boosts)] conj
                           {:acc hyp :rej conflicts})))))

(defn add-fact
  [workspace hyp]
  (prof :add-fact
        (if (prof :add-fact-dup-check ((:hyp-contents workspace) (:contents hyp)))
          workspace
          (-> (add workspace hyp)
              (update-in [:forced] conj (:id hyp))
              (update-in [:accepted (:type hyp)] conj (:id hyp))
              (update-in [:accepted :all] conj (:id hyp))
              (assoc-needing-explanation hyp)))))

(defn get-unexplained
  [workspace]
  (prof :get-unexplained
        (map #(lookup-hyp workspace %) (:sorted-explainers-explained workspace))))

(defn get-unexp-pct
  [workspace]
  (prof :get-unexp-pct
        (if (empty? (:sorted-explainers-explained workspace)) 0.0
            (/ (double (count (:sorted-explainers-explained workspace)))
               (double (count (filter :needs-explainer?
                                 (map #(lookup-hyp workspace %)
                                    (:all (:accepted workspace))))))))))

(defn get-noexp-pct
  [workspace]
  (prof :get-noexp-pct
        (if (empty? (filter :needs-explainer?
                            (map #(lookup-hyp workspace %)
                                 (:all (:accepted workspace)))))
          0.0
          (/ (double (count (find-no-explainers workspace)))
             (double (count (filter :needs-explainer?
                                    (map #(lookup-hyp workspace %)
                                         (:all (:accepted workspace))))))))))

(defn calc-doubt
  [workspace]
  (prof :calc-doubt
        (let [acc-not-forced (filter #(not ((:forced workspace) %))
                                (:all (:accepted workspace)))]
          (if (empty? acc-not-forced)
            (if (empty? (:sorted-explainers-explained workspace)) 0.0 1.0)
            (let [confs (vals (select-keys (:hyp-confidences workspace) acc-not-forced))]
              (/ (reduce + 0.0 (map #(- 1.0 %) confs)) (count confs)))))))

(defn calc-coverage
  [workspace]
  (prof :calc-coverage
        (let [forced (set (map #(lookup-hyp workspace %)
                               (:forced workspace)))
              accepted (set (map #(lookup-hyp workspace %)
                                 (:all (:accepted workspace))))
              accessible (set
                          (mapcat (fn [h] (map #(lookup-hyp workspace %)
                                               (pre-traverse (:graph workspace) (:id h))))
                                  (set/difference accepted forced)))]
          (/ (double (count (set/intersection forced accessible)))
             (double (count forced))))))

(defn find-unaccepted
  [workspace]
  (prof :find-unaccepted
        (set/difference
         (set (map #(lookup-hyp workspace %)
                   (apply concat (vals (:hypotheses workspace)))))
         (set (map #(lookup-hyp workspace %)
                   (:all (:accepted workspace)))))))

(defn find-best
  [workspace]
  (prof :find-best
        (let [threshold (/ (:Threshold params) 100.0)
              essentialid (first (filter #(nil? (second (get (:sorted-explainers workspace) %)))
                                    (:sorted-explainers-explained workspace)))]
          (if essentialid
            (let [essential (lookup-hyp workspace essentialid) 
                  bestid (first (get (:sorted-explainers workspace) essentialid))
                  best (lookup-hyp workspace bestid)]
              {:best best :essential? true
               :explained essential :choices []})
            ;; otherwise, choose highest-delta non-essential
            (let [explid (first (:sorted-explainers-explained workspace))
                  expl (lookup-hyp workspace explid)
                  choices (map #(lookup-hyp workspace %)
                             (get (:sorted-explainers workspace) explid))
                  best (first choices)
                  nbest (second choices)
                  delta (- (hyp-conf workspace best)
                           (hyp-conf workspace nbest))]            
              (when (>= delta threshold)
                {:best best :essential? false :delta delta
                 :explained expl :alts (rest choices)}))))))

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
                      (assoc-in [:accepted :kb] (map :id new-kb-hyps))
                      (update-in [:accepted :all] set/union (set (map :id new-kb-hyps))))))))

(defn get-explaining-hypotheses
  [workspace]
  (prof :get-explaining-hypotheses
        ((:hypothesize-fn (:abduction @problem))
         (map #(lookup-hyp workspace %)
            (:sorted-explainers-explained workspace))
         (:accepted workspace) (partial lookup-hyp workspace))))

(defn update-hypotheses
  [workspace]
  (prof :update-hypotheses
        (let [hyps (get-explaining-hypotheses workspace)]
          (reduce add workspace hyps))))

(defn explain
  [workspace]
  (prof :explain
        ;; need (loop) because we are using (recur) which isn't going
        ;; to work when profiling is on
        (loop [workspace workspace]
          (log "Explaining again...")
          (let [ws-explainers (if (or (:dirty workspace)
                                      (not= "none" (:ConfAdjustment params)))
                                (update-sorted-explainers workspace)
                                workspace)]
            (if (empty? (:sorted-explainers-explained ws-explainers))
              (do (log "No explainers. Done.")
                  ws-explainers)
              (let [{:keys [best essential? explained delta] :as b}
                    (find-best ws-explainers)]
                (if-not best
                  (do (log "No best. Done.")
                      ws-explainers)
                  (do (log "Best is" (:id best) (hyp-conf ws-explainers best))
                      (let [ws-accepted
                            (let [ws-logged (-> ws-explainers
                                               (update-in [:cycle] inc)
                                               (update-in [:log :best] conj b))]
                              (accept ws-logged best explained delta essential?))]
                        (recur ws-accepted))))))))))

(comment
  (defn revise
    [workspace hyp]
    ;; don't 'revise' if already accepted this hyp
    (if (some #{(:id hyp)} (get-in workspace [:accepted (:type hyp)])) workspace
        ;; here is "belief revision" in all its glory
        (let [conflicts (filter (set (map #(lookup-hyp workspace %)
                                          (apply concat (vals (:accepted workspace)))))
                                (map #(lookup-hyp workspace %) (:conflicts hyp)))]
          #_(println "revising with" hyp "conflicts" conflicts)
          (-> (reduce (fn [ws h] (update-in ws [:accepted (:type h)] disj (:id h)))
                      workspace conflicts)
              (add hyp)
              (accept hyp [] nil false)
              (assoc :prior-explainers nil)
              (explain))))))

(defn update-graph
  [workspace]
  (if @batch workspace
      (prof :update-graph
            (let [g-conflicts
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
                   (:graph workspace)
                   (apply concat (vals (:hypotheses workspace))))
                  g-accepted
                  (reduce
                   (fn [g h]
                     (-> g (add-attr h :fontcolor "green")
                         (add-attr h :color "green")))
                   g-conflicts (:all (:accepted workspace)))
                  g-facts
                  (reduce
                   (fn [g h]
                     (-> g (add-attr h :fontcolor "gray50")
                         (add-attr h :color "gray50")))
                   g-accepted (:forced workspace))]
              (assoc workspace :graph g-facts)))))

(defn add-sensor-hyps
  [workspace time-prev time-now sensors]
  (prof :add-sensor-hyps
        (let [hs (mapcat (fn [s] ((:make-sensor-hyps-fn (:abduction @problem))
                                 s time-prev time-now (:hypotheses workspace)))
                         sensors)]
          (reduce add-fact workspace hs))))

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
        (let [kb-hyps (map #(lookup-hyp workspace %)
                         (get-in workspace [:hypotheses :kb]))]
          (-> workspace
             (update-in [:hyp-contents] set/difference (set (map :contents kb-hyps)))
             (assoc-in [:accepted :kb] '())
             (assoc-in [:hypotheses :kb] '())))))

(defn init-kb
  [workspace training]
  (prof :init-kb
        (add-kb workspace ((:generate-kb-fn (:abduction @problem)) training))))

(defn reset-workspace
  [workspace]
  (prof :reset-workspace
        (add-kb (assoc empty-workspace :oracle (:oracle workspace)
                       :oracle-types (:oracle-types workspace)
                       :scores (:scores workspace)
                       :score-adjustments (:score-adjustments workspace))
                (map #(lookup-hyp workspace %)
                   (get-in workspace [:accepted :kb])))))

(defn init-workspace
  []
  (prof :init-workspace
        (assoc empty-workspace
          :oracle-types
          (set (map keyword (str/split (:Oracle params) #","))))))

(defn extract-training
  [ws-orig ws-trained]
  (prof :extract-training
        (add-kb (remove-kb (assoc ws-orig :scores (:scores ws-trained)
                                  :score-adjustments (:score-adjustments ws-trained)))
                (map #(lookup-hyp ws-trained %)
                   (get-in ws-trained [:accepted :kb])))))

(defn inject-true-hyps
  [workspace true-false-types]
  (prof :inject-true-hyps
        (reduce (fn [ws h]
             (-> ws
                (update-in [:accepted (:type h)] conj (:id h))
                (update-in [:hypotheses (:type h)] conj (:id h))
                (update-in [:hypotheses :all] conj (:id h))
                (assoc-in [:hyp-ids (:id h)] h)))
           (-> workspace
              (assoc :sorted-explainers {}
                     :sorted-explainers-explained '())
              (update-in [:accepted :all]
                         (set/union (set (map :id (get-in true-false-types [:all true]))))))
           (get-in true-false-types [:all true]))))
