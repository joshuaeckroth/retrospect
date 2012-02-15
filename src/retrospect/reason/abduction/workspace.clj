(ns retrospect.reason.abduction.workspace
  (:use [retrospect.confidences])
  (:require [clojure.set :as set])
  (:use [loom.io :only [dot-str]])
  (:use [loom.graph :only
         [digraph nodes incoming neighbors weight
          add-nodes add-edges remove-nodes edges transpose]])
  (:use [loom.alg :only [pre-traverse]])
  (:use [loom.attr :only [add-attr remove-attr]])
  (:use [clojure.contrib.combinatorics :only [combinations]])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.random])
  (:use [retrospect.state]))

(defrecord Hypothesis
    [id type subtype needs-explainer?
     conflict apriori explains depends desc]
  Object
  (toString [self] (format "%s: %s" id desc))
  Comparable
  (compareTo [self other] (compare (hash self) (hash other))))

(defmethod print-method Hypothesis
  [o w]
  (print-simple (:id o) w))

(defn new-hyp
  [prefix type subtype needs-explainer?
   conflict apriori explains depends desc data]
  (let [id (inc last-id)]
    (set-last-id id)
    ;; use var-set if running batch mode; def if using player or repl
    ;; (in batch mode, thread is called something like pool-2-thread-1)

    (merge
     (Hypothesis. (format "%s%d" prefix id)
                  type subtype needs-explainer? conflict
                  apriori explains depends desc)
     data)))

(defn hyp-log
  [workspace hyp]
  (get (:hyp-log workspace) hyp))

(defn hyp-conf
  [workspace hyp]
  (get (:hyp-confidences workspace) hyp))

(defn find-no-explainers
  [workspace]
  (set (filter #(empty? (get (:explainers workspace) %))
               (:needs-explainer workspace))))

(defn find-active-explains
  [workspace hyp]
  (filter (:active-explainers workspace) (:explains hyp)))

(defn compare-by-conf
  "Since we are using probabilities, smaller value = less confidence. We want
   most confident first. If confidences are equal, hyps that explain more are
   moved first. Otherwise, comparison is done by the :id's."
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
                                      (sort (partial compare-by-conf workspace)
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
                        (keys (:active-explainers workspace))))))

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
                        ;; average the normalized conf with existing conf
                        (assoc-in ws2 [:hyp-confidences hyp]
                                  (/ (+ (hyp-conf ws2 hyp) (get norm-alts hyp)) 2.0)))
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
  (find-conflicts-selected workspace hyp (apply concat (vals (:accepted workspace)))))

(defn add-explainers
  [workspace hyp]
  (reduce (fn [ws h]
            (let [ws2 (update-in ws [:explainers h] conj hyp)]
              ;; don't add active-explainers to hyps that have already
              ;; been fully explained (indicated by no set of
              ;; active-explainers)
              (if-not (get (:active-explainers ws2) h) ws2
                (update-in ws2 [:active-explainers h] conj hyp))))
          workspace (:explains hyp)))

(defn remove-explainer
  [workspace hyp]
  (println "Removing explainers for" hyp)
  (reduce (fn [ws h] (let [new-ae (update-in (:active-explainers ws) [h] disj hyp)]
                       (if (empty? (get new-ae h))
                         (assoc ws :active-explainers (dissoc new-ae h))
                         (assoc ws :active-explainers new-ae))))
          workspace (:explains hyp)))

(defn reject-many
  [workspace hyps]
  (let [rejectable (filter (fn [h] (not-any? #(= (:id %) (:id h))
                                             (get (:accepted workspace) (:type h))))
                           hyps)]
    (println "Rejecting" hyps)
    (-> (reduce (fn [ws hyp]
                  (-> ws
                      (remove-explainer hyp)
                      (update-in [:active-explainers] dissoc hyp)
                      (update-in [:needs-explainer] disj hyp)
                      (update-in [:rejected (:type hyp)] conj hyp)
                      (update-in [:hyp-log hyp] conj
                                 (format "Rejected in cycle %d" (:cycle workspace)))))
                workspace rejectable)
        (update-in [:graph]
                   #(reduce (fn [g r] (-> g (add-attr r :fontcolor "red")
                                          (add-attr r :color "red")))
                            % rejectable)))))

(defn add
  [workspace hyp]
  (println "Adding" hyp)
  (let [g-added (reduce (fn [g n] (-> g (add-nodes n)
                                      (add-attr n :id (:id n))
                                      (add-attr n :label (:id n))))
                        (:graph workspace)
                        (conj (:explains hyp) hyp))
        g-edges (reduce (fn [g e] (add-edges g [hyp e]))
                        g-added (:explains hyp))
        ws (-> (add-explainers workspace hyp)
               (assoc :graph g-edges)
               (assoc-in [:hyp-confidences hyp] (:apriori hyp))
               (update-in [:hypotheses (:type hyp)] conj hyp))
        ws-needs-explainer (if-not (:needs-explainer? hyp) ws
                                   (-> ws (update-in [:needs-explainer] conj hyp)
                                       (assoc-in [:active-explainers hyp] #{})))
        conflicts (find-conflicts ws-needs-explainer hyp)]
    (if (empty? conflicts) ws-needs-explainer
        (reject-many ws-needs-explainer [hyp]))))

(defn accept
  [workspace hyp alts]
  (println "Accepting" hyp)
  (let [commas (fn [ss] (apply str (interpose ", " (sort ss))))
        ws (-> workspace
               (update-in
                [:hyp-log hyp] conj
                (format "Accepted in cycle %d (alts: %s)"
                        (:cycle workspace) (commas (map :id alts))))
               (update-in [:accepted (:type hyp)] conj hyp)
               (update-in [:graph] add-attr hyp :fontcolor "green")
               (update-in [:graph] add-attr hyp :color "green"))
        ws-expl (reduce (fn [ws2 h] (update-in ws2 [:active-explainers] dissoc h))
                        ws (:explains hyp))
        ws-alts (reduce (fn [ws2 alt] (update-in ws2 [:hyp-log alt] conj
                                                 (format "Alternate in cycle %d"
                                                         (:cycle workspace))))
                        ws-expl alts)
        conflicts (find-conflicts ws-alts hyp)
        ws-conflicts (reject-many ws-alts conflicts)]
    (update-in ws-conflicts [:log :accrej (:cycle workspace)] conj
               {:acc hyp :rej conflicts})))

(defn add-fact
  [workspace hyp]
  (-> (add workspace hyp)
      (update-in [:forced] conj hyp)
      (update-in [:accepted (:type hyp)] conj hyp)
      (update-in [:graph] add-attr hyp :fontcolor "gray50")
      (update-in [:graph] add-attr hyp :color "gray50")))

(defn measure-doubt
  [workspace]
  (let [acc-not-forced (filter #(not ((:forced workspace) %))
                               (apply concat (vals (:accepted workspace))))]
    (if (empty? acc-not-forced)
      (if (empty? (:unexplained (:log workspace))) 0.0 1.0)
      (let [confs (vals (select-keys (:hyp-confidences workspace) acc-not-forced))]
        (double (/ (reduce + 0.0 (map #(- 1.0 %) confs)) (count confs)))))))

(defn get-unexp-pct
  "Only measure unexplained \"needs-explainer\" hyps."
  [workspace]
  (if (empty? (:forced workspace)) 0.0
      (double (/ (count (filter (:needs-explainer workspace)
                                (:unexplained (:log workspace))))
                 (count (:needs-explainer workspace))))))

(defn get-noexp-pct
  [workspace]
  (if (empty? (:needs-explainer workspace)) 0.0
      (double (/ (count (:no-explainers (:log workspace)))
                 (count (:needs-explainer workspace))))))

(defn log-final
  [workspace explainers]
  (let [ws (update-in workspace [:log] merge
                      {:unexplained (filter (:needs-explainer workspace)
                                            (keys (:active-explainers workspace)))
                       :no-explainers (find-no-explainers workspace)
                       :unaccepted (set/difference
                                    (set (apply concat (vals (:hypotheses workspace))))
                                    (set (apply concat (vals (:accepted workspace))))
                                    (set (apply concat (vals (:rejected workspace)))))
                       :last-explainers explainers})]
    (assoc ws :doubt (measure-doubt ws))))

(defn find-best
  [workspace explainers threshold]
  (if (empty? explainers) {}
      (let [essentials (filter #(= 1 (count (:expl %))) explainers)]
        (if (not-empty essentials)
          ;; choose most confident/most-explaining essential
          (let [expl (first essentials)
                best (first (:expl expl))]
            (println "Choosing best (essential)" best)
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
              (println "Choosing best" best "delta" delta)
              {:best best :alts (rest alts)
               :essential? false :delta delta :explained (:hyp expl)}))))))

(defn make-more-hyp
  [evidence apriori]
  (new-hyp "?" :more (:type evidence) false nil apriori evidence [] "" {}))

(defn remove-hyp
  [workspace hyp]
  (println "Removing" hyp)
  (-> workspace
      (remove-explainer hyp)
      (update-in [:graph] remove-nodes hyp)
      (update-in [:hyp-confidences] dissoc hyp)
      (update-in [:hypotheses (:type hyp)] (fn [hs] (filter #(not= % hyp) hs)))
      (update-in [:active-explainers] dissoc hyp)
      (update-in [:explainers] dissoc hyp)))

(defn get-more-hyps
  ;;TODO figure out order hyps should be attempted
  ([workspace]
     (get-more-hyps workspace (my-shuffle (keys (:active-explainers workspace)))))
  ([workspace hyps]
     (println "Getting more hyps...")
     (loop [hs hyps]
       (when (not-empty hs)
         (let [expl ((:hypothesize-fn (:abduction @problem)) (first hs)
                     (:accepted workspace) (:rejected workspace)
                     (:hypotheses workspace))]
           (if expl
             (if (nil? (second expl)) [(first expl)]
                 [(first expl) (make-more-hyp (ffirst expl) (second (second expl)))])
             (recur (rest hs))))))))

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
             (println "Need more hyps?" answer)
             answer))
  false
  )

(defn explain
  [workspace]
  (loop [ws workspace]
    (if (empty? (:active-explainers ws))
      (do (println "No more active explainers") (log-final ws []))
        (if-let [hs (and (need-more-hyps? ws) (get-more-hyps ws))]
          (recur (reduce add ws hs))
          (let [explainers (find-all-explainers ws)]
            (println "Explainers:" explainers)
            (if (empty? explainers)
              (if-let [hs (get-more-hyps ws)]
                (recur (reduce add ws hs))
                (log-final ws []))
              (let [ws-confs (update-confidences ws explainers)
                    explainers-sorted (sort-explainers ws-confs explainers)
                    {:keys [best alts essential? delta] :as b}
                    (find-best ws-confs explainers-sorted
                               (/ (:Threshold params) 100.0))]
                (if-not best
                  (if-let [hs (get-more-hyps ws-confs)]
                    (recur (reduce add ws-confs hs))
                    (log-final ws-confs explainers-sorted))
                  (if (= (:type best) :more)
                    (if-let [hs (get-more-hyps ws-confs (:explains best))]
                      (recur (remove-hyp (reduce add ws-confs hs) best))
                      (recur (remove-hyp ws-confs best)))
                    (recur
                     (let [ws-logged (-> ws-confs
                                         (update-in [:cycle] inc)
                                         (update-in [:log :best] conj b))]
                       (accept ws-logged best alts))))))))))))

(defn analyze
  [workspace hyp]
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
                ws-altered (-> workspace (update-in [:accepted] set/difference check)
                               (reject-many check))
                ws-explained (explain ws-altered)
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

(defn add-sensor-hyps
  [workspace time-prev time-now sensors]
  (let [msh (fn [s h t] ((:make-sensor-hyps-fn (:abduction @problem))
                         s h t time-prev time-now))]
    (reduce (fn [ws t]
              (let [hs (mapcat (fn [s] (mapcat #(msh s % t) (sensed-at s t))) sensors)]
                (reduce add-fact ws hs)))
            workspace (range (inc time-prev) (inc time-now)))))

(defn add-kb
  [training ws]
  (reduce (fn [ws2 h] (-> ws2 (add h) (update-in [:accepted (:type h)] conj h)))
          ws ((:generate-kb-fn (:abduction @problem)) training)))

(defn init-workspace
  [training]
  (add-kb training
          {:graph (digraph)
           :cycle 0
           :log {:unexplained [] :no-explainers [] :unaccepted []
                 :best [] :accrej {}}
           :hyp-log {}
           :doubt nil
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
           :rejected {}}))
