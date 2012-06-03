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
    [id type subtype needs-explainer? conflicts?-fn explains boosts short-str desc data]
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
   ;; hyp type => hyp subtype => score
   :scores {}
   :log {:unexplained [] :best [] :accrej {}}
   :hyp-log {}
   ;; a list of hyps that explain each key;
   ;; hyps that are keys need to be explained
   :active-explainers {}
   ;; all explainers, keyed by hyp-id, with vals as sets of hyp-ids; serves as a cache
   :explainers {}
   ;; a map of hyp-id => hyp
   :hyp-ids {}
   ;; a map of type => seq
   :hypotheses {}
   ;; a set of hypothesis :data + :type maps (for dup searching)
   :hyp-contents #{}
   ;; hyp-ids that are neither accepted nor rejected
   :available #{}
   ;; keyed by hyp-id
   :hyp-confidences {}
   ;; set of hyp-ids
   :forced #{}
   ;; a map of type => seq, with additional key :all
   :accepted {}})

(defn new-hyp
  [prefix type subtype needs-explainer? conflicts?-fn explains boosts short-str desc data]
  (let [id (inc last-id)]
    (set-last-id id)
    (assoc
        (merge (Hypothesis.
                (format "%s%d" prefix id)
                type subtype needs-explainer? conflicts?-fn explains
                boosts short-str desc data)
               data)
      :contents (assoc data :type type :subtype subtype))))

(defn lookup-hyp
  [workspace id]
  (prof :lookup-hyp
        (get-in workspace [:hyp-ids id])))

(defn accepted?
  [workspace hyp]
  (prof :accepted?
        (some #{(:id hyp)} (get-in workspace [:accepted (:type hyp)]))))

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
        (set (filter (fn [h] (empty? (get (:explainers workspace) (:id h))))
                     (filter :needs-explainer?
                             (map #(lookup-hyp workspace %)
                                  (:all (:accepted workspace))))))))

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

(defn find-all-explainers
  [workspace]
  (let [expl-filter (fn [hypid] ((:available workspace) hypid))]
    (prof :find-all-explainers
          (sort-by (comp :id first)
                   (doall (filter
                           (comp first :expl)
                           (map
                            (fn [h]
                              {:hyp h
                               :expl (doall (map #(lookup-hyp workspace %)
                                                 (filter expl-filter
                                                         (get (:active-explainers workspace)
                                                              (:id h)))))})
                            (map #(lookup-hyp workspace %)
                                 (keys (:active-explainers workspace))))))))))

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

(defn boost
  [workspace hyp]
  (if (or (not (:ApplyBoosting params))
          (not ((:available workspace) (:id hyp))))
    workspace
    (let [new-conf (min 1.0 (+ 0.10 (hyp-conf workspace hyp)))]
      (-> workspace
          (assoc-in [:hyp-confidences (:id hyp)] new-conf)
          (update-in [:hyp-log hyp] conj
                     (format "Confidence boosted in cycle %d from %.2f to %.2f"
                             (:cycle workspace) (hyp-conf workspace hyp) new-conf))))))

(defn find-conflicts-all
  [workspace hyp]
  (prof :find-conflicts-all
        (doall (filter (partial (:conflicts?-fn hyp) hyp) (vals (:hyp-ids workspace))))))

(defn find-conflicts
  [workspace hyp]
  (prof :find-conflicts
        (doall (filter (partial (:conflicts?-fn hyp) hyp)
                       (map #(lookup-hyp workspace %) (:available workspace))))))

(defn add-explainers
  [workspace hyp]
  (prof :add-explainers
        (reduce (fn [ws h]
                  (let [ws2 (update-in ws [:explainers (:id h)] conj (:id hyp))]
                    ;; don't add active-explainers to hyps that have already
                    ;; been fully explained (indicated by not present in
                    ;; :active-explainers)
                    (if-not (get (:active-explainers ws2) (:id h)) ws2
                            (update-in ws2 [:active-explainers (:id h)] conj (:id hyp)))))
                workspace (explains hyp))))

(defn remove-explainers
  [workspace hyp]
  (prof :remove-explainers
        (do
          (log "Removing explainers for" hyp)
          (reduce (fn [ws h] (if (nil? (get (:active-explainers ws) (:id h))) ws
                                 (update-in ws [:active-explainers (:id h)] disj (:id hyp))))
                  workspace (explains hyp)))))

(defn reject-many
  [workspace hyps]
  (prof :reject-many
        (reduce
         (fn [ws hyp]
           (let [ws2 (-> ws
                         (remove-explainers hyp)
                         (update-in [:active-explainers] dissoc (:id hyp))
                         (update-in [:available] disj (:id hyp)))]
             (if @batch ws2
                 (update-in ws2 [:hyp-log hyp] conj
                            (format "Rejected in cycle %d"
                                    (:cycle workspace))))))
         workspace hyps)))

(defn lookup-score
  [workspace hyp]
  (prof :lookup-score
        (get-in workspace [:scores (:type hyp) (:subtype hyp)] 0.5)))

(defn add
  [workspace hyp]
  (prof :add
        (if (prof :add-dup-search ((:hyp-contents workspace) (:contents hyp)))
          workspace
          (let [ws-needs-explainer
                (prof :add-needs-explainer
                      (if-not
                          (prof :active-explainers
                                (get (:active-explainers workspace) (:id hyp))) workspace
                                (assoc-in workspace [:active-explainers (:id hyp)] #{})))
                ws-explainers
                (prof :add-ws-update
                      (-> ws-needs-explainer
                          (update-in [:hyp-ids] assoc (:id hyp) hyp)
                          (update-in [:hyp-contents] conj (:contents hyp))
                          (update-in [:available] conj (:id hyp))
                          (add-explainers hyp)
                          (assoc-in
                           [:hyp-confidences (:id hyp)]
                           (cond ((:oracle-types workspace) (:type hyp))
                                 (if ((:oracle workspace) hyp) 1.0 0.0)
                                 :else
                                 (lookup-score workspace hyp)))
                          (update-in [:hypotheses (:type hyp)] conj (:id hyp))))]
            (if @batch ws-explainers
                (prof :add-graph-update
                      (let [g-added (reduce (fn [g h]
                                              (-> g (add-nodes (:id h))
                                                  (add-attr (:id h) :id (:id h))
                                                  (add-attr (:id h) :label (:short-str h))))
                                            (:graph ws-explainers)
                                            (conj (explains hyp) hyp))
                            g-expl (reduce (fn [g e] (add-edges g [(:id hyp) (:id e)]))
                                           g-added (explains hyp))]
                        (assoc ws-explainers :graph g-expl))))))))

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

(defn accept
  [workspace hyp explained delta essential?]
  (prof :accept
        (let [ws-needs-exp
              (prof :accept-needs-exp
                    (if-not ((:explainers workspace) (:id hyp)) workspace
                            (assoc-in workspace [:active-explainers (:id hyp)]
                                      (set ((:explainers workspace) (:id hyp))))))
              ws-acc (prof :accept-update
                           (-> ws-needs-exp
                               (update-in [:accepted (:type hyp)] conj (:id hyp))
                               (update-in [:accepted :all] conj (:id hyp))
                               (update-in [:available] disj (:id hyp))))
              ws-hyplog (if @batch ws-acc
                            (update-in ws-acc [:hyp-log hyp] conj
                                       (format (str "Accepted in cycle %d "
                                                    "to explain %s with delta %.2f"
                                                    " (essential? %s)")
                                               (:cycle workspace)
                                               explained delta essential?)))
              ws-expl (prof :accept-expl
                            (reduce (fn [ws2 h]
                                      (update-in ws2 [:active-explainers] dissoc (:id h)))
                                    ws-hyplog (explains hyp)))
              conflicts (prof :accept-conflicts (find-conflicts ws-expl hyp))
              ws-conflicts (prof :accept-reject-many (reject-many ws-expl conflicts))
              ws-boosts (prof :accept-boosts (reduce boost ws-conflicts (:boosts hyp)))]
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
              (assoc-in [:active-explainers (:id hyp)] #{})))))

(defn get-unexp-pct
  "Only measure unexplained \"needs-explainer\" hyps."
  [workspace]
  (prof :get-unexp-pct
        (if (empty? (filter :needs-explainer?
                            (map #(lookup-hyp workspace %)
                                 (:all (:accepted workspace)))))
          0.0
          (/ (double (count (:unexplained (:log workspace))))
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
            (if (empty? (:unexplained (:log workspace))) 0.0 1.0)
            (let [confs (vals (select-keys (:hyp-confidences workspace) acc-not-forced))]
              (reduce + 0.0 (map #(- 1.0 %) confs)))))))

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

(defn log-final
  [workspace explainers]
  (prof :log-final
        (-> workspace
            (update-in [:log] merge
                       {:unexplained (map #(lookup-hyp workspace %)
                                          (keys (:active-explainers workspace)))
                        :last-explainers explainers})
            (dissoc :prior-explainers))))

(defn find-best
  [workspace explainers threshold]
  (prof :find-best
        (if (empty? explainers) {}
            (let [essential (first (filter #(nil? (second (:expl %))) explainers))]
              (if essential
                (let [best (first (:expl essential))]
                  {:best best :essential? true
                   :explained (:hyp essential) :choices []})
                ;; otherwise, choose highest-delta non-essential
                (let [expl (first explainers)
                      best (first (:expl expl))
                      next-best (second (:expl expl))
                      delta (- (hyp-conf workspace best)
                               (hyp-conf workspace next-best))]            
                  (when (>= delta threshold)
                    {:best best :essential? false :delta delta
                     :explained (:hyp expl) :alts (rest (:expl expl))})))))))

(defn update-kb
  [workspace]
  (prof :update-kb
        (if-not (:UpdateKB params) workspace
                (let [new-kb-hyps ((:update-kb-fn (:abduction @problem))
                                   (:accepted workspace)
                                   (:unexplained (:log workspace))
                                   (:hypotheses workspace)
                                   (partial lookup-hyp workspace))]
                  (-> (reduce (fn [ws h] (assoc-in ws [:hyp-ids (:id h)] h))
                              workspace new-kb-hyps)
                      (assoc-in [:accepted :kb] (map :id new-kb-hyps))
                      (update-in [:accepted :all] concat (map :id new-kb-hyps)))))))

(defn update-hypotheses
  [workspace]
  (let [hyps ((:hypothesize-fn (:abduction @problem))
              (map #(lookup-hyp workspace %) (:forced workspace))
              (:accepted workspace) (partial lookup-hyp workspace))]
    (reduce add workspace hyps)))

(defn explainers-prior-filter
  [workspace]
  (prof :explain-prior-filter
        (doall
         (filter (comp first :expl)
                 (map (fn [es]
                        (assoc es :expl
                               (filter #((:available workspace) (:id %))
                                       (:expl es))))
                      (filter #((:active-explainers workspace) (:id (:hyp %)))
                              (:prior-explainers workspace)))))))

(defn explain
  [workspace]
  (prof :explain
        (loop [ws workspace]
          (log "Explaining again...")
          (let [explainers (if (and (= "none" (:ConfAdjustment params))
                                    (:prior-explainers ws))
                             (explainers-prior-filter ws)
                             (find-all-explainers ws))]
            (if (empty? explainers)
              (do (log "No explainers. Done.")
                  (log-final ws []))
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
                      (log-final ws-confs explainers-sorted))
                  (do (log "Best is" (:id best) (hyp-conf ws-confs best))
                      (let [ws-accepted
                            (let [ws-logged (-> ws-confs
                                                (update-in [:cycle] inc)
                                                (update-in [:log :best] conj b))]
                              (accept ws-logged best explained delta essential?))]
                        (recur (assoc ws-accepted :prior-explainers explainers-sorted)))))))))))

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

(defn add-sensor-hyps
  [workspace time-prev time-now sensors]
  (let [hs (mapcat (fn [s] ((:make-sensor-hyps-fn (:abduction @problem))
                            s time-prev time-now (:hypotheses workspace)))
                   sensors)]
    (reduce add-fact workspace hs)))

(defn add-kb
  [workspace hyps]
  (reduce (fn [ws h]
            (-> ws (add h)
                (update-in [:accepted (:type h)] conj (:id h))
                (update-in [:accepted :all] conj (:id h))))
          workspace hyps))

(defn init-kb
  [workspace training]
  (add-kb workspace ((:generate-kb-fn (:abduction @problem)) training)))

(defn reset-workspace
  [workspace]
  (add-kb (assoc empty-workspace :oracle (:oracle workspace)
                 :oracle-types (:oracle-types workspace)
                 :scores (:scores workspace))
          (map #(lookup-hyp workspace %)
               (get-in workspace [:accepted :kb]))))

(defn init-workspace
  ([] (assoc empty-workspace
        :oracle-types
        (set (map keyword (str/split (:Oracle params) #","))))))

(defn extract-training
  [ws-orig ws-trained]
  (add-kb (assoc ws-orig :scores (:scores ws-trained))
          (map #(lookup-hyp ws-trained %)
               (get-in ws-trained [:accepted :kb]))))
