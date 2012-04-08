(ns retrospect.reason.abduction.workspace
  (:import (misc AlphanumComparator))
  (:use [retrospect.confidences])
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:use [loom.io :only [dot-str]])
  (:use [loom.graph :only
         [digraph nodes incoming neighbors weight
          add-nodes add-edges remove-nodes edges transpose]])
  (:use [loom.alg :only [pre-traverse]])
  (:use [loom.attr :only [add-attr remove-attr]])
  (:use [clojure.contrib.combinatorics :only [combinations]])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.logging])
  (:use [retrospect.random])
  (:use [retrospect.state]))

(defrecord Hypothesis
    [id type subtype needs-explainer?
     conflict apriori explains depends short-str desc]
  Object
  (toString [self] (format "%s(%s)" id short-str))
  Comparable
  (compareTo [self other] (compare (hash self) (hash other))))

(defmethod print-method Hypothesis
  [o w]
  (print-simple (format "%s(%s)" (:id o) (:short-str o)) w))

(def empty-workspace
  {:graph (digraph)
   :cycle 0
   ;; remember the order hyps were added
   :added []
   :initial-kb []
   :log {:unexplained [] :no-explainers [] :unaccepted []
         :best [] :accrej {}}
   :hyp-log {}
   :doubt nil
   :coverage nil
   ;; a list of hyps that explain each key;
   ;; hyps that are keys need to be explained
   :active-explainers {}
   ;; all explainers; serves as a cache
   :explainers {}
   ;; a map of type => seq
   :hypotheses {}
   :hyp-confidences {}    
   :needs-explainer #{}
   :forced #{}
   ;; a map of type => seq
   :accepted {}
   ;; a map of type => seq
   :rejected {}})

(defn new-hyp
  [prefix type subtype needs-explainer?
   conflict apriori explains depends short-str desc data]
  (let [id (inc last-id)]
    (set-last-id id)
    ;; use var-set if running batch mode; def if using player or repl
    ;; (in batch mode, thread is called something like pool-2-thread-1)

    (merge
     (Hypothesis. (format "%s%d" prefix id)
                  type subtype needs-explainer? conflict
                  apriori explains depends short-str desc)
     data)))

(defn hyp-log
  [workspace hyp]
  (get (:hyp-log workspace) hyp))

(defn hyp-conf
  [workspace hyp]
  (if-not (:UseScores params) 0.0 (get (:hyp-confidences workspace) hyp)))

(defn find-no-explainers
  [workspace]
  (set (filter (fn [h] (empty? (filter #(not= :more (:type %))
                                       (get (:explainers workspace) h))))
               (:needs-explainer workspace))))

(defn find-active-explains
  [workspace hyp]
  (filter (:active-explainers workspace) (:explains hyp)))

(defn compare-by-conf-expl
  "Since we are using probabilities, smaller value = less
   confidence. We want most confident first, then number of hyps that
   each explains. Otherwise, comparison is done by the :id's (to keep
   it deterministic)."
  [workspace hyp1 hyp2]
  (let [conf (- (compare (hyp-conf workspace hyp1)
                                 (hyp-conf workspace hyp2)))
        expl (- (compare (count (find-active-explains workspace hyp1))
                         (count (find-active-explains workspace hyp2))))

        id (compare (:id hyp1) (:id hyp2))]
    (if (= 0 conf)
      (if (= 0 expl) id expl)
      conf)))

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
        (- (compare (hyp-conf workspace (first hyps1))
                    (hyp-conf workspace (first hyps2)))))
      (- (compare hyps1-delta hyps2-delta)))))

(defn sort-explainers
  [workspace explainers]
  (let [internal-sorted (map (fn [expl]
                               (assoc expl :expl
                                      (sort (partial compare-by-conf-expl workspace)
                                            (:expl expl))))
                             explainers)]
    (sort (fn [expl1 expl2]
            (compare-by-delta workspace (:expl expl1) (:expl expl2)))
          internal-sorted)))

(defn find-all-explainers
  [workspace]
  (sort-by (comp :id first)
           (filter (comp first :expl)
                   (map (fn [h] {:hyp h :expl (get (:active-explainers workspace) h)})
                        (:needs-explainer workspace)))))

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
  (reduce (fn [ws {hyp :hyp alts :expl}]
            (let [norm-alts (normalize-confidences alts)]
              ;; for each normalized confidence hyp
              (reduce (fn [ws2 hyp]
                        ;; take the min normalized conf with existing conf
                        (assoc-in ws2 [:hyp-confidences hyp]
                                  (min (hyp-conf ws2 hyp) (get norm-alts hyp))))
                      ws (sort-by :id (keys norm-alts)))))
          workspace explainers))

(defn find-conflicts-selected
  "If a hypothesis's :conflict value is a function (a predicate), that function
   is called with the hyp and each other hyp. If the :conflict value is
   a keyword, the conflicts of a hyp are those other hyps that share the
   :conflict keyword"
  [workspace hyp hyps]
  (let [c (:conflict hyp)]
    (cond
     ;; no conflict id; so it conflicts with nothing
     (nil? c) []

     ;; we have a function (predicate), so call the function on other hyps
     (fn? c) (filter #(and (not= % hyp) (c hyp %)) hyps)

     ;; otherwise, hyps conflict if their conflict ids are identical
     :else
     (filter #(and (not= % hyp) (= c (:conflict %))) hyps))))

(defn find-conflicts
  [workspace hyp]
  (find-conflicts-selected workspace hyp (apply concat (vals (:hypotheses workspace)))))

(defn add-explainers
  [workspace hyp]
  (reduce (fn [ws h]
            (let [ws2 (update-in ws [:explainers h] conj hyp)]
              ;; don't add active-explainers to hyps that have already
              ;; been fully explained (indicated by not present in
              ;; :active-explainers)
              (if-not (get (:active-explainers ws2) h) ws2
                (update-in ws2 [:active-explainers h] conj hyp))))
          workspace (:explains hyp)))

(defn remove-explainers
  [workspace hyp]
  (log "Removing explainers for" (:id hyp))
  (reduce (fn [ws h] (if (nil? (get (:active-explainers ws) h)) ws
                         (update-in ws [:active-explainers h] disj hyp)))
          workspace (:explains hyp)))

(defn reject-many
  [workspace hyps]
  (let [rejectable (filter (fn [h] (not-any? #(= (:id %) (:id h))
                                             (concat (get (:accepted workspace) (:type h))
                                                     (get (:rejected workspace) (:type h)))))
                           hyps)]
    (when (not-empty rejectable) (log "Rejecting" (str/join ", " (map :id rejectable))))
    (-> (reduce (fn [ws hyp]
                  (-> ws
                      (remove-explainers hyp)
                      (update-in [:active-explainers] dissoc hyp)
                      (update-in [:needs-explainer] disj hyp)
                      (update-in [:rejected (:type hyp)] conj hyp)
                      (update-in [:hyp-log hyp] conj
                                 (format "Rejected in cycle %d" (:cycle workspace)))))
                workspace rejectable)
        (update-in [:graph] #(reduce (fn [g r] (-> g (add-attr r :fontcolor "red")
                                                   (add-attr r :color "red")))
                                     % rejectable)))))

(defn add
  [workspace hyp]
  (let [g-added (reduce (fn [g n] (-> g (add-nodes n)
                                      (add-attr n :id (:id n))
                                      (add-attr n :label (:id n))))
                        (:graph workspace)
                        (conj (:explains hyp) hyp))
        g-edges (reduce (fn [g e] (add-edges g [hyp e]))
                        g-added (:explains hyp))
        ws-needs-explainer (if-not (:needs-explainer? hyp) workspace
                                   (assoc-in workspace [:active-explainers hyp] #{}))
        ws-explainers (-> ws-needs-explainer
                          (update-in [:added] conj hyp)
                          (add-explainers hyp)
                          (assoc :graph g-edges)
                          ;; put 1.0 in the confidences map; it will be adjusted later
                          (assoc-in [:hyp-confidences hyp] 1.0)
                          (update-in [:hypotheses (:type hyp)] conj hyp))
        conflicts (find-conflicts-selected ws-explainers hyp
                                           (apply concat (vals (:accepted ws-explainers))))]
    (if (empty? conflicts) ws-explainers
        (reject-many ws-explainers [hyp]))))

(defn accept
  [workspace hyp alts]
  (if (some #(= (:id hyp) (:id %)) (get (:accepted workspace) (:type hyp))) workspace
      (do (log "Accepting" (:id hyp))
          (let [commas (fn [ss] (apply str (interpose ", " (sort ss))))
                ;; recursively accept explained hyps
                ws workspace #_(reduce (fn [ws2 h] (accept ws2 h [])) workspace (:explains hyp))
                ws-needs-exp (if-not (:needs-explainer? hyp) ws
                                     (update-in ws [:needs-explainer] conj hyp))
                ws-acc (-> ws-needs-exp
                           (update-in [:hyp-log hyp] conj
                                      (format "Accepted in cycle %d (alts: %s)"
                                              (:cycle workspace) (commas (map :id alts))))
                           (update-in [:accepted (:type hyp)] conj hyp)
                           (update-in [:graph] add-attr hyp :fontcolor "green")
                           (update-in [:graph] add-attr hyp :color "green"))
                ws-expl (reduce (fn [ws2 h]
                                  (-> ws2 (update-in [:needs-explainer] disj h)
                                      (update-in [:active-explainers] dissoc h)))
                                ws-acc (:explains hyp))
                ws-alts (reduce (fn [ws2 alt] (update-in ws2 [:hyp-log alt] conj
                                                         (format "Alternate in cycle %d"
                                                                 (:cycle ws2))))
                                ws-expl alts)
                conflicts (find-conflicts ws-alts hyp)
                ws-conflicts (reject-many ws-alts conflicts)]
            (update-in ws-conflicts [:log :accrej (:cycle ws-conflicts)] conj
                       {:acc hyp :rej conflicts})))))

(defn add-fact
  [workspace hyp]
  (-> (add workspace hyp)
      (update-in [:forced] conj hyp)
      (update-in [:accepted (:type hyp)] conj hyp)
      (update-in [:needs-explainer] conj hyp)
      (update-in [:graph] add-attr hyp :fontcolor "gray50")
      (update-in [:graph] add-attr hyp :color "gray50")))

(defn get-unexp-pct
  "Only measure unexplained \"needs-explainer\" hyps."
  [workspace]
  (if (empty? (:forced workspace)) 0.0
      (/ (double (count (:unexplained (:log workspace))))
         (double (count (filter #(:needs-explainer? %)
                                (apply concat (vals (:accepted workspace)))))))))

(defn get-noexp-pct
  [workspace]
  (if (empty? (:needs-explainer workspace)) 0.0
      (/ (double (count (:no-explainers (:log workspace))))
         (double (count (filter #(:needs-explainer? %)
                                (apply concat (vals (:accepted workspace)))))))))

(defn measure-doubt
  [workspace]
  (let [acc-not-forced (filter #(not ((:forced workspace) %))
                               (apply concat (vals (:accepted workspace))))]
    (if (empty? acc-not-forced)
      (if (empty? (:unexplained (:log workspace))) 0.0 1.0)
      (let [confs (vals (select-keys (:hyp-confidences workspace) acc-not-forced))]
        (reduce + 0.0 (map #(- 1.0 %) confs))))))

(defn measure-coverage
  [workspace]
  (let [accessible (set (mapcat (fn [h] (pre-traverse (:graph workspace) h))
                                (set/difference (set (apply concat (vals (:accepted workspace))))
                                                (:forced workspace))))]
    (/ (double (count (set/intersection (:forced workspace) accessible)))
       (double (count (:forced workspace))))))

(defn log-final
  [workspace explainers]
  (let [ws (update-in workspace [:log] merge
                      {:unexplained (:needs-explainer workspace)
                       :no-explainers (find-no-explainers workspace)
                       :unaccepted (set/difference
                                    (set (apply concat (vals (:hypotheses workspace))))
                                    (set (apply concat (vals (:accepted workspace))))
                                    (set (apply concat (vals (:rejected workspace)))))
                       :last-explainers explainers})]
    (-> ws
        (assoc :doubt (measure-doubt ws))
        (assoc :coverage (measure-coverage ws)))))

(defn find-best
  [workspace explainers threshold]
  (if (empty? explainers) {}
      (let [essentials (filter #(= 1 (count (:expl %))) explainers)]
        (if (not-empty essentials)
          ;; choose most confident/most-explaining essential
          (let [expl (first essentials)
                best (first (:expl expl))]
            (log "Choosing best (essential)" (:id best))
            {:best best
             :alts (disj (set (mapcat :expl (rest essentials))) best)
             :essential? true :delta nil :explained (:hyp expl)})
          ;; otherwise, choose highest-delta non-essential
          (let [expl (first explainers)
                alts (:expl expl)
                best (first alts)
                delta (- (hyp-conf workspace (first alts))
                         (hyp-conf workspace (second alts)))]            
            (when (>= delta threshold)
              (log "Choosing best" (:id best) "delta" delta)
              {:best best :alts (rest alts)
               :essential? false :delta delta :explained (:hyp expl)}))))))

(defn make-more-hyp
  [expl apriori]
  (new-hyp "?" :more (:type (first expl)) false nil apriori expl [] "" "" {}))

(defn make-more-learn-hyp
  [expl apriori]
  (new-hyp "L?" :learn-more (:type (first expl)) false nil apriori expl [] "" "" {}))

(defn remove-hyp
  [workspace hyp]
  (log "Removing" (:id hyp))
  (-> workspace
      (remove-explainers hyp)
      (update-in [:graph] remove-nodes hyp)
      (update-in [:hyp-confidences] dissoc hyp)
      (update-in [:hypotheses (:type hyp)] (fn [hs] (filter #(not= % hyp) hs)))
      (update-in [:active-explainers] dissoc hyp)
      (update-in [:needs-explainer] disj hyp)
      (update-in [:explainers] dissoc hyp)))

(defn get-another-hyp
  ([workspace]
     ;;TODO figure out order hyps should be attempted
     (get-another-hyp workspace (sort-by :id (AlphanumComparator.)
                                         (:needs-explainer workspace))))
  ([workspace unexp]
     (when (not-empty unexp)
       (log "Getting more hyps for" (str/join "," (map :id unexp)))
       (let [hyps (loop [hs unexp
                         h-map (:hypotheses workspace)
                         expl []]
                    (if (empty? hs) expl
                        (do (log "Trying to get an explainer for" (str (first hs)))
                            (let [es ((:hypothesize-fn (:abduction @problem)) (first hs)
                                      (:accepted workspace) (:rejected workspace)
                                      h-map)]
                              (log "Got:" (str/join "," (map str es)))
                              (recur (rest hs)
                                     (if (not-empty es)
                                       (reduce #(update-in %1 [(:type %2)] conj %2)
                                               h-map es)
                                       h-map)
                                     (concat expl (or es [])))))))]
         (when (not-empty hyps) hyps)))))

(defn get-learn-hyp
  [workspace]
  (let [unexp (sort-by :id (AlphanumComparator.) (:needs-explainer workspace))]
    (when (not-empty unexp)
      (log "Getting learning hyps for unexp" (str/join "," (map :id unexp)))
      (let [hyps (loop [hs unexp
                        h-map (:hypotheses workspace)
                        expl []]
                   (if (empty? hs) expl
                       (do (log "Trying to get a learn explainer for" (str (first hs)))
                           (let [es ((:learn-fn (:abduction @problem))
                                     (first hs) unexp h-map)]
                             (log "Got:" (str/join "," (map str es)))
                             (recur (rest hs)
                                    (if (not-empty es)
                                      (reduce #(update-in %1 [(:type %2)] conj %2)
                                              h-map es)
                                      h-map)
                                    (concat expl (or es [])))))))]
        (when (not-empty hyps) hyps)))))

(defn need-more-hyps?
  [workspace]
  ;; every hyp that has an explainer, also has an explainer that
  ;; explains something already accepted that's not also forced
  (comment (let [answer
                 (every? (fn [h]
                           ;; h is a hyp that is an active explainer
                           (some (fn [h2]
                                   ;; h2 is a hyp that is actively explained
                                   (some                           
                                    (fn [h3]
                                      ;; h3 is a hyp that h2 explains
                                      (and (not ((:forced workspace) h3))
                                           (not-empty
                                            (filter #(= (:id h3) (:id %))
                                                    (get (:accepted workspace) (:type h3))))))
                                    (:explains h2)))
                                 (get (:active-explainers workspace) h)))
                         (filter #(not-empty (get (:active-explainers workspace) %))
                                 (keys (:active-explainers workspace))))]
             (log "Need more hyps?" answer)
             answer))
  false
  )

(defn reset-workspace
  [workspace]
  (let [added (:added workspace)
        forced (:forced workspace)
        learned (:learned workspace)]
    (assoc (reduce (fn [ws h] (if (forced h) (add-fact ws h) (add ws h)))
                   (assoc empty-workspace :initial-kb (:initial-kb workspace))
                   added)
      :learned learned)))

(defn explain
  [workspace]
  (loop [ws workspace]
    (log "Explaining again...")
    (let [explainers (find-all-explainers ws)]
      (if (empty? explainers)
        (do (log "No explainers. Attempting to get more hyps...")
            (if-let [hs (get-another-hyp ws)]
              (recur (reduce add ws hs))
              (if (or (not (:Learn params)) (:learned ws))
                (do (log "Can't get more hyps, already learned or learning disabled. Done.")
                    (log-final ws []))
                (do (log "No more hyps. Attempting to learn...")
                    (if-let [hs (get-learn-hyp ws)]
                      (recur (assoc (reduce add (reset-workspace ws) hs) :learned true))
                      (do (log "Nothing to learn. Done.")
                          (log-final ws [])))))))
        (let [ws-confs (update-confidences ws explainers)
              explainers-sorted (sort-explainers ws-confs explainers)
              {:keys [best alts essential? delta] :as b}
              (find-best ws-confs explainers-sorted
                         (/ (:Threshold params) 100.0))]
          (if-not best
            (do (log "No best.")
                (if-let [hs (get-another-hyp ws-confs)]
                  (recur (reduce add ws-confs hs))
                  (log-final ws-confs explainers-sorted)))
            (if (= (:type best) :more)
              (do (log "Best is :more hyp.")
                  (if-let [hs (get-another-hyp ws-confs (:explains best))]
                    (recur (remove-hyp (reduce add ws-confs hs) best))
                    (recur (remove-hyp ws-confs best))))
              (do (log "Best is" (:id best) (hyp-conf ws-confs best))
                  (let [ws-accepted (let [ws-logged (-> ws-confs
                                                        (update-in [:cycle] inc)
                                                        (update-in [:log :best] conj b))]
                                      (accept ws-logged best alts))]
                    (if (>= (double (:DoubtThreshold params)) (measure-doubt ws-accepted))
                      (recur ws-accepted)
                      (do (log "Doubt threshold would be surpassed by accepting best. Done.")
                          (log-final ws-confs explainers-sorted))))))))))))

(defn add-sensor-hyps
  [workspace time-prev time-now sensors]
  (let [msh (fn [s h t] ((:make-sensor-hyps-fn (:abduction @problem))
                         s h t time-prev time-now))]
    (reduce (fn [ws t]
              (let [hs (mapcat (fn [s] (mapcat #(msh s % t) (sensed-at s t))) sensors)]
                (reduce add-fact ws hs)))
            workspace
            (range (if (:ResensePrevTime params) time-prev (inc time-prev))
                   (inc time-now)))))

(defn add-kb
  [workspace hyps]
  (reduce (fn [ws h] (-> ws (add h) (update-in [:accepted (:type h)] conj h)
                         (update-in [:initial-kb] conj h)))
          workspace hyps))

(defn init-kb
  [workspace training]
  (add-kb workspace ((:generate-kb-fn (:abduction @problem)) training)))

(defn init-workspace
  ([] (when-let [f (:reset-fn (:abduction @problem))] (f))
     empty-workspace)
  ([workspace]
     (when-let [f (:reset-fn (:abduction @problem))] (f))
     (add-kb empty-workspace (:initial-kb workspace))))
