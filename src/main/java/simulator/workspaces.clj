(ns simulator.workspaces
  (:require [simulator logs])
  (:import [simulator.logs AbducerLogEntry HypLogEntry])
  (:require [simulator.hypotheses :as hyps :only
         [init-hypspace get-explainers get-hyp-id-str get-hyp-ids-str
          set-confidence penalize boost find-conflicts find-unexplained
          get-confidence add-explainers get-apriori add-conflicts
          find-essentials find-best delete-hyps]])
  (:use [simulator.confidences])
  (:require [clojure.set :as set]))

(defrecord Workspace
    [hypspace
     accepted
     hypothesized
     unexplained
     decision
     abducer-log
     hyp-log])

(defn init-workspace
  []
  (Workspace. (hyps/init-hypspace) [] [] []
              {:confidence nil :hyps [] :forced []}
              [] {}))

(defn add-abducer-log-msg
  [workspace hyps msg]
  (let [entry (AbducerLogEntry. (map hyps/get-hyp-id-str hyps) msg)]
    (update-in workspace [:abducer-log] conj entry)))

(defn add-hyp-log-msg
  [workspace hyp msg]
  (let [entry (HypLogEntry. (hyps/get-hyp-id-str hyp) msg)]
    (if (get (:hyp-log workspace) hyp)
      (update-in workspace [:hyp-log hyp] conj entry)
      (update-in workspace [:hyp-log] assoc hyp [entry]))))

(defn log-final-accepted-hyps
  [workspace]
  (let [confstr (fn [h] (confidence-str (hyps/get-confidence (:hypspace workspace) h)))]
    (add-abducer-log-msg
     workspace (:hyps (:decision workspace))
     (format "Final accepted hyps: %s"
             (apply str
                    (interpose ", "
                               (map #(format "%s(%s)" (hyps/get-hyp-id-str %) (confstr %))
                                    (:hyps (:decision workspace)))))))))

(defn accept-workspace-decision
  [workspace]
  (let [accepted (concat (:accepted workspace)
                         (:hyps (:decision workspace))
                         (:forced (:decision workspace)))]
    (-> (init-workspace)
        (assoc :hypspace (:hypspace workspace))
        (assoc :accepted accepted)
        (assoc :unexplained (hyps/find-unexplained (:hypspace workspace) accepted)))))

(defn measure-decision-confidence
  [workspace]
  ;; if no accepted hyps, this is very implausible
  (if (empty? (:hyps (:decision workspace))) VERY-IMPLAUSIBLE
      ;; if accepted hyps exist, find the minimum confidence of them
      (apply min (map (fn [h] (hyps/get-confidence (:hypspace workspace) h))
                      (:hyps (:decision workspace))))))

(defn update-decision-confidence
  [workspace]
  (update-in workspace [:decision] assoc :confidence
             (measure-decision-confidence workspace)))

(defn get-decision-confidence
  [workspace]
  (:confidence (:decision workspace)))

(defn clear-decision
  [workspace]
  "Clear the decision, except for what was 'forced'."
  (assoc workspace :decision {:confidence nil :hyps []
                              :forced (:forced (:decision workspace))}))

(defn penalize-decision-hyps
  [workspace hyps]
  (if (empty? hyps) workspace
      (let [hypspace (reduce (fn [hs h] (hyps/penalize hs h))
                             (:hypspace workspace) hyps)
            ws (-> workspace
                   (assoc :hypspace hypspace)
                   (add-abducer-log-msg hyps
                                        (format "Penalizing reverted decision: %s."
                                                (hyps/get-hyp-ids-str hyps))))]
        (reduce (fn [ws h] (add-hyp-log-msg ws h "Penalizing reverted decision."))
                ws hyps))))

(defn delete-random-min-conf-decision
  [workspace]
  (if (empty? (:hyps (:decision workspace))) workspace
      (let [hyp (first (sort-by #(hyps/get-confidence (:hypspace workspace) %)
                                (:hyps (:decision workspace))))
            ws-deleted (assoc workspace :hypspace
                              (hyps/delete-hyps (:hypspace workspace) [hyp]))
            ws-hyp-logs (add-hyp-log-msg ws-deleted hyp
                                         (format "Deleting as part of reverted
                                                  decision since its confidence %s
                                                  is lowest among the decision hyps."
                                                 (confidence-str
                                                  (hyps/get-confidence (:hypspace workspace)
                                                                       hyp))))]
        (add-abducer-log-msg
         ws-hyp-logs [hyp]
         (format "Deleting random hyp in decision that has lowest confidence: %s"
                 (hyps/get-hyp-id-str hyp))))))

(defn reset-confidences-to-apriori
  [workspace]
  (let [hyps (:hyps (:decision workspace))
        hypspace (reduce (fn [hs h] (hyps/set-confidence hs h (hyps/get-apriori h)))
                         (:hypspace workspace) hyps)]
    (reduce (fn [ws h] (add-hyp-log-msg ws h "Resetting confidence back to apriori value."))
            (-> workspace
                (assoc :hypspace hypspace)
                (add-abducer-log-msg hyps "Resetting confidence back to apriori values."))
            hyps)))

(defn add-hyp
  "explained must be a sequence."
  [workspace hyp explained]
  (let [hypspace (-> (:hypspace workspace)
		     (update-in [:hyps] conj hyp)
		     (hyps/add-explainers explained [hyp])
		     (hyps/set-confidence hyp (hyps/get-apriori hyp)))]
    (-> workspace
	(update-in [:hypothesized] conj hyp)
	(assoc :hypspace hypspace)
        (add-abducer-log-msg [hyp] "Adding hypothesis.")
        (add-hyp-log-msg hyp "Adding hypothesis."))))

(defn add-mutual-conflicts
  [workspace hyps]
  (reduce (fn [ws hyp] (update-in ws [:hypspace] hyps/add-conflicts hyp
				  (set/difference hyps #{hyp})))
	  workspace hyps))

(defn add-mutual-conflicts-all-explainers
  [workspace hyp]
  (add-mutual-conflicts workspace (hyps/get-explainers (:hypspace workspace) hyp)))

(defn penalize-conflicts
  [workspace conflicts log-msg]
  (if (empty? conflicts) workspace
    (let [hypspace (reduce (fn [hs c] (hyps/penalize hs c))
                           (:hypspace workspace) conflicts)
          ws (-> workspace
                 (assoc :hypspace hypspace)
                 (add-abducer-log-msg conflicts
                                      (format "Penalizing conflicts: %s."
                                              (hyps/get-hyp-ids-str conflicts))))]
      (reduce (fn [ws c] (add-hyp-log-msg ws c log-msg))
               ws conflicts))))

(defn accept-hyp
  [workspace hyp]
  (let [conflicts (hyps/find-conflicts (:hypspace workspace) #{hyp})]
    (-> workspace
        (update-in [:decision :hyps] conj hyp)
        (penalize-conflicts conflicts
                            (format "Penalizing due to conflict with accepted hyp %s."
                                    (hyps/get-hyp-id-str hyp))))))

(defn accept-hyp-forced
  [workspace hyp]
  (update-in workspace [:decision :forced] conj hyp))

(defn force-acceptance
  [workspace hyp]
  (-> workspace
      (accept-hyp-forced hyp)
      (add-abducer-log-msg
       [hyp] (format "Forcing acceptance of: %s." (hyps/get-hyp-id-str hyp)))
      (add-hyp-log-msg hyp "Forcing acceptance.")))

(defn accept-explainer-type
  [workspace explainer hyp type]
  (-> workspace
      (add-abducer-log-msg
       [hyp explainer] (format "Accepting %s %s as explainer of %s."
                               type
                               (hyps/get-hyp-id-str explainer)
                               (hyps/get-hyp-id-str hyp)))
      (add-hyp-log-msg
       explainer (format "Accepting as %s explainer of %s."
                         type (hyps/get-hyp-id-str hyp)))
      (add-hyp-log-msg
       hyp (format "Hyp %s accepted as %s explainer."
                   (hyps/get-hyp-id-str explainer) type))
      (accept-hyp explainer)))

(defn choose-random-hyp
  ([hyps] (rand-nth (vec hyps)))
     
  ([type workspace hyps]
     (cond (= :smartguess type)
           (let [hs (:hypspace workspace)
                 threshold (first (sort (map (fn [h] (hyps/get-confidence hs h))
                                             hyps)))]
             (rand-nth (vec (filter (fn [h] (= threshold (hyps/get-confidence hs h))) hyps))))
           
           :else
           (choose-random-hyp hyps))))

(defn unexplained-helper
  [workspace]
  (hyps/find-unexplained (:hypspace workspace)
                         (concat
                          (:forced (:decision workspace))
                          (:hyps (:decision workspace))
                          (:accepted workspace))))

(defn get-explainers
  [workspace hyp]
  (hyps/get-explainers (:hypspace workspace) hyp))

(defn get-explains
  [workspace hyp]
  (hyps/get-explains (:hypspace workspace) hyp))

(defn find-essentials
  [workspace]
  (hyps/find-essentials (:hypspace workspace) (unexplained-helper workspace)))

(defn find-best
  [workspace threshold type]
  (hyps/find-best (:hypspace workspace) (unexplained-helper workspace) threshold type))