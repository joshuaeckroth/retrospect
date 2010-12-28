(ns simulator.workspaces
  (:require [simulator logs])
  (:import [simulator.logs AbducerLogEntry])
  (:use [simulator.confidences])
  (:require [clojure.set :as set]))

(defrecord Hypothesis
    [id ;; a keyword
     type ;; a keyword
     apriori
     confidence
     explains
     implausible-fn
     impossible-fn
     update-fn
     ancient-fn
     str-fn
     data])

(defrecord Workspace
    [hyps ;; this is a map keyed by hyp-id
     accepted ;; hyp-ids
     rejected ;; hyp-ids
     hypothesized ;; hyp-ids
     unexplained ;; hyp-ids
     candidates ;; hyp-ids
     abducer-log
     decision]) ;; {:confidence :accepted (hyp-ids) :rejected (hyp-ids) :forced (hyp-ids)}

(defn init-workspace
  []
  (Workspace. {} [] [] [] [] [] []
              {:confidence nil :accepted [] :rejected [] :forced []}))

(defn update-hyps
  [workspace hyps]
  (doall (reduce (fn [ws h] (update-in ws [:hyps] assoc (:id h) h))
                 workspace hyps)))

(defn lookup-hyps
  [workspace hyp-ids]
  (doall (map #((:hyps workspace) %) hyp-ids)))

(defn add-abducer-log-msg
  [workspace hyps msg]
  (update-in workspace [:abducer-log] conj (AbducerLogEntry. hyps msg)))

(defn log-final-accepted-rejected-hyps
  [workspace]
  (let [id-to-str (fn [t] (apply str (interpose ", " (map name (t (:decision workspace))))))]
    (add-abducer-log-msg
     workspace
     (concat (:accepted (:decision workspace))
             (:rejected (:decision workspace)))
     (format "Final accepted: %s\n\tFinal rejected: %s\n\tFinal unexplained: %s"
             (id-to-str :accepted) (id-to-str :rejected)
             (apply str (interpose ", " (map name (:unexplained workspace))))))))

(defn find-explainers
  [hyp hyps]
  (doall (filter (fn [h] (some #(= (:id hyp) %) (:explains h))) hyps)))

(defn delete-ancient-hyps
  "Called by OneRun."
  [workspace time]
  (let [ancient #{} ; (set (map :id (filter #((:ancient-fn %) % time) (vals (:hyps workspace)))))
        new-hyps (apply dissoc (:hyps workspace) ancient)
        new-accepted (set/difference (set (:accepted workspace)) ancient)
        new-rejected (set/difference (set (:rejected workspace)) ancient)
        new-candidates (set/difference (set (:candidates workspace)) ancient)]
    (-> workspace
        (assoc :hyps new-hyps)
        (assoc :accepted new-accepted)
        (assoc :rejected new-rejected)
        (assoc :candidates new-candidates))))

(defn update-candidates-unexplained
  [workspace]
  (let [accepted-ids (concat
                      (:accepted workspace)
                      (:accepted (:decision workspace))
                      (:forced (:decision workspace)))
        non-accepted-ids (set/difference
                      (set (keys (:hyps workspace)))
                      (set (concat accepted-ids
                                   (:rejected workspace)
                                   (:rejected (:decision workspace)))))
        accepted (lookup-hyps workspace accepted-ids)
        non-accepted (lookup-hyps workspace non-accepted-ids)
        is-unexplained #(and (empty? (find-explainers % accepted))
                             (not-empty (find-explainers % non-accepted)))]
    (-> workspace
        (assoc :candidates non-accepted-ids)
        (assoc :unexplained (map :id (filter is-unexplained accepted))))))

(defn accept-workspace-decision
  [workspace]
  (let [accepted (concat (:accepted workspace)
                         (:accepted (:decision workspace))
                         (:forced (:decision workspace)))
        rejected (concat (:rejected workspace)
                         (:rejected (:decision workspace)))]
    (-> (init-workspace)
        (assoc :hyps (:hyps workspace))
        (assoc :accepted accepted)
        (assoc :rejected rejected)
        (update-candidates-unexplained))))

(defn measure-decision-confidence
  [workspace]
  ;; if no accepted hyps, this is very implausible
  (if (empty? (:accepted (:decision workspace))) VERY-IMPLAUSIBLE
      ;; if accepted hyps exist, find the minimum confidence of them
      (apply min (map (fn [h] (:confidence h))
                      (lookup-hyps workspace (:accepted (:decision workspace)))))))

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
  (update-in workspace [:decision]
             assoc :confidence nil :accepted [] :rejected []))

(defn reset-confidences-to-apriori
  [workspace]
  (-> workspace
      (update-hyps (doall (map #(assoc % :confidence (:apriori %)) (vals (:hyps workspace)))))
      (add-abducer-log-msg (keys (:hyps workspace))
                           "Resetting confidences back to apriori values.")))

(defn make-next-hyp-id
  "Make a hypothesis id that is just the next value from the largest
  existing id."
  [workspace]
  (format "%d" (inc (apply max 0 (map #(Integer/parseInt (name %))
                                      (filter #(re-matches #"\d+" (name %))
                                              (keys (:hyps workspace))))))))

(defn add-hyp
  "Add the hypothesis to the workspace. If the hyp has no :id field,
  then one is generated."
  [workspace hyp]
  (let [id (if (:id hyp) (:id hyp) (make-next-hyp-id workspace))
        hyp-with-id (assoc hyp :id id)]
    (-> workspace
        (update-in [:hypothesized] conj id)
        (update-in [:hyps] assoc id hyp-with-id)
        (add-abducer-log-msg
         [id] (format "Adding hypothesis (apriori=%s; explains %s)."
                      (confidence-str (:apriori hyp-with-id))
                      (apply str (interpose "," (map name (:explains hyp-with-id))))))
        (update-candidates-unexplained))))

(defn penalize-implausible
  [workspace hyps log-msg]
  (if (empty? hyps) workspace
      (let [penalized (doall (map #(update-in % [:confidence] penalize) hyps))]
        (add-abducer-log-msg (update-hyps workspace penalized) (map :id penalized) log-msg))))

(defn reject-impossible
  [workspace hyps log-msg]
  (if (empty? hyps) workspace
      (let [rejected (doall (map #(assoc % :confidence IMPOSSIBLE) hyps))]
        (-> workspace
            (update-hyps rejected)
            (update-in [:decision :rejected] concat (map :id rejected))
            (add-abducer-log-msg (map :id rejected) log-msg)))))

(defn reject-all-impossible
  [workspace]
  (let [impossible (filter #(= (:confidence %) IMPOSSIBLE)
                           (lookup-hyps workspace (:candidates workspace)))]
    (-> workspace
        (reject-impossible impossible "Rejecting due to IMPOSSIBLE confidence.")
        (update-candidates-unexplained))))

(defn accept-hyp
  [workspace hyp]
  (let [implausible (doall ((:implausible-fn hyp) hyp (vals (:hyps workspace))))
        impossible (doall ((:impossible-fn hyp) hyp (vals (:hyps workspace))))]
    (-> workspace
        (update-in [:decision :accepted] conj (:id hyp))
        (penalize-implausible
         implausible
         (format "Penalizing because accepting %s." (name (:id hyp))))
        (reject-impossible
         impossible
         (format "Rejecting because accepting %s." (name (:id hyp))))
        (update-candidates-unexplained))))

(defn force-acceptance
  [workspace hyp]
  (-> workspace
      (update-in [:decision :forced] conj (:id hyp))
      (add-abducer-log-msg
       [(:id hyp)] (format "Forcing acceptance of %s." (name (:id hyp))))
      (update-candidates-unexplained)))

(defn find-best
  [workspace]
  (let [unexplained (lookup-hyps workspace (:unexplained workspace))
        candidates (lookup-hyps workspace (:candidates workspace))
        explainers (map #(find-explainers % candidates) unexplained)
        essentials (filter #(= 1 (count %)) explainers)]
    (if (not-empty essentials)
      ;; choose most confident essential
      (first (reverse (sort-by :confidence (apply concat essentials))))

      ;; otherwise choose random most confident / most explanatory
      (let [sorted (reverse (sort-by :confidence (apply concat explainers)))
            max-conf (:confidence (first sorted))
            most-conf (filter #(= max-conf (:confidence %)) sorted)
            expl-sorted (reverse (sort-by (comp count :explains) most-conf))
            max-expl (count (:explains (first expl-sorted)))]
        (if (not-empty most-conf)
          (rand-nth (vec (filter #(= max-expl (count (:explains %))) most-conf))))))))

(defn explain
  [workspace]
  (let [ws (reject-all-impossible workspace)]
    (if (empty? (:unexplained ws)) ws
        (let [best (find-best ws)]
          (if (nil? best) ws
              (recur (-> ws
                         (add-abducer-log-msg
                          (conj (:explains best) (:id best))
                          (format "Accepting %s as explainer of %s." (name (:id best))
                                  (apply str (interpose ", " (map name (:explains best))))))
                         (accept-hyp best))))))))

