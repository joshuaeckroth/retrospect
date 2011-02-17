(ns samre.workspaces
  (:require [samre logs])
  (:import [samre.logs AbducerLogEntry])
  (:use [samre.confidences])
  (:require [clojure.set :as set])
  (:require [clojure.string :as string]))

(defrecord Hypothesis
    [id ;; a keyword
     type ;; a keyword
     apriori
     confidence
     explains
     implausible-fn
     impossible-fn
     str-fn
     data]
  Object
  (toString [this] (str-fn this)))

(defn new-hyp
  [id type apriori explains implausible-fn impossible-fn str-fn data]
  (Hypothesis. id type apriori apriori
               explains implausible-fn impossible-fn str-fn data))

(defrecord Workspace
    [hyps ;; this is a map keyed by hyp-id
     accepted ;; hyp-ids
     rejected ;; hyp-ids
     hypothesized ;; hyp-ids
     unexplained ;; hyp-ids
     candidates ;; hyp-ids
     abducer-log
     dot ;; graph vector
     decision]) ;; {:confidence :accepted (hyp-ids) :rejected (hyp-ids) :forced (hyp-ids)}

(defn init-workspace
  []
  (Workspace. {} [] [] [] [] [] [] []
              {:confidence nil :accepted [] :rejected [] :forced []}))

(defn update-hyps
  [workspace hyps]
  (doall (reduce (fn [ws h] (update-in ws [:hyps] assoc (:id h) h))
                 workspace hyps)))

(defn lookup-hyps
  [workspace hyp-ids]
  (doall (filter identity (map #((:hyps workspace) %) hyp-ids))))

(defn add-abducer-log-msg
  [workspace hyps msg]
  (update-in workspace [:abducer-log] conj (AbducerLogEntry. hyps msg)))

(defn log-final-accepted-rejected-hyps
  [workspace]
  (let [id-to-str
        (fn [t] (apply str (interpose ", " (sort (map name (t (:decision workspace)))))))]
    (add-abducer-log-msg
     workspace
     (concat (:accepted (:decision workspace))
             (:rejected (:decision workspace)))
     (format "Final accepted: %s\n\tFinal rejected: %s\n\tFinal unexplained: %s"
             (id-to-str :accepted) (id-to-str :rejected)
             (apply str (interpose ", " (sort (map name (:unexplained workspace)))))))))

(defn find-explainers
  [hyp hyps]
  (doall (filter (fn [h] (some #(= (:id hyp) %) (:explains h))) hyps)))

(defn find-ancient-hyps
  "An 'ancient' hyp is one that is not explained by something
   currently unexplained or a candidate hyp."
  [workspace]
  (let [active-hyps (lookup-hyps workspace (concat (:candidates workspace)
                                                   (:unexplained workspace)))
        explains (filter #(% (:hyps workspace)) (flatten (map :explains active-hyps)))]
    (filter (fn [hid] (not-any? #(= hid (:id %)) explains))
            (keys (:hyps workspace)))))

(defn delete-ancient-hyps
  "Called by new-child-ep-state in epistemicstates.clj"
  [workspace]
  (let [ancient (set (find-ancient-hyps workspace))
        new-hyps (apply dissoc (:hyps workspace) ancient)
        new-accepted (set/difference (set (:accepted workspace)) ancient)
        new-rejected (set/difference (set (:rejected workspace)) ancient)
        new-candidates (set/difference (set (:candidates workspace)) ancient)]
    (-> workspace
        (assoc :dot [])
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
        ;; a hyp is unexplained if it could be explained or it's forced
        ;; but, in either case, is not yet explained
        is-unexplained #(and (empty? (find-explainers % accepted))
                             (or (some (fn [h] (= h (:id %))) (:forced (:decision workspace)))
                                 (not-empty (find-explainers % non-accepted))))
        unexplained (map :id (filter is-unexplained accepted))]
    (-> workspace
        (assoc :candidates non-accepted-ids)
        (assoc :unexplained unexplained))))

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

(defn add-hyp
  "Add the hypothesis to the workspace."
  [workspace hyp]
  ;; don't add an identical existing hyp
  (if (get (:hyps workspace) (:id hyp)) workspace
      (let [explains (map name (filter #(% (:hyps workspace)) (:explains hyp)))]
        (-> workspace
            (update-in [:hypothesized] conj (:id hyp))
            (update-in [:hyps] assoc (:id hyp) hyp)
            (add-abducer-log-msg
             [(:id hyp)]
             (format "Adding hypothesis (apriori=%s; explains %s)."
                     (confidence-str (:apriori hyp))
                     (apply str (interpose "," explains))))))))

(defn penalize-implausible
  [workspace hyps log-msg]
  (if (empty? hyps) workspace
      (let [penalized (doall (map #(update-in % [:confidence] penalize) hyps))]
        (add-abducer-log-msg (update-hyps workspace penalized) (map :id penalized) log-msg))))

(defn reject-impossible
  [workspace hyps log-msg]
  (if (empty? hyps) workspace
      ;; only reject those that are not already rejected
      (let [rejected (doall (map #(assoc % :confidence IMPOSSIBLE)
                                 (filter #(not= IMPOSSIBLE (:confidence %)) hyps)))]
        (-> workspace
            (update-hyps rejected)
            (update-in [:decision :rejected] concat (map :id rejected))
            (add-abducer-log-msg (map :id rejected) log-msg)))))

(defn reject-all-impossible
  [workspace]
  (let [impossible (filter #(= (:confidence %) IMPOSSIBLE)
                           (lookup-hyps workspace (:candidates workspace)))]
    (-> workspace
        (reject-impossible impossible "Rejecting due to IMPOSSIBLE confidence."))))

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
         (format "Rejecting because accepting %s." (name (:id hyp)))))))

(defn force-acceptance
  [workspace hyp]
  (-> workspace
      (update-in [:decision :forced] conj (:id hyp))
      (add-abducer-log-msg
       [(:id hyp)] (format "Forcing acceptance of %s." (name (:id hyp))))))

(defn dot-format
  [workspace boxed acc]
  (let [id #(format "%s %s" (name (:id %)) (confidence-str (:confidence %)))
        all-acc (lookup-hyps workspace (concat (:accepted (:decision workspace))
                                               (:forced (:decision workspace))
                                               (:accepted workspace)))
        all-rej (lookup-hyps workspace (concat (:rejected (:decision workspace))
                                               (:rejected workspace)))]
    (str
     "digraph G {\n"
     "rankdir=\"LR\";\n"
     "node [shape=\"plaintext\"];\n"
     (apply str (for [h (vals (:hyps workspace))]
                  (str (apply str (map #(format "\"%s\" -> \"%s\";\n" (id h) (id %))
                                       (lookup-hyps workspace (:explains h)))))))
     (if (empty? all-acc) ""
         (apply str (for [h all-acc]
                      (format "\"%s\" [color=\"blue\", fontcolor=\"blue\"];\n"
                              (id h)))))
     (if (empty? all-rej) ""
         (apply str (for [h all-rej]
                      (format "\"%s\" [color=\"red\", fontcolor=\"red\"];\n"
                              (id h)))))
     (if (empty? (:unexplained workspace)) ""
         (apply str (for [h (lookup-hyps workspace (:unexplained workspace))]
                      (format "\"%s\" [color=\"orange\", fontcolor=\"orange\"];\n"
                              (id h)))))
     "subgraph cluster {\n"
     (if (= 0 (count boxed)) ""
       (if (< 0 (count boxed))
         (apply str (map #(format "\"%s\";\n" (id %)) boxed))
         (format "\"%s\";\n" (id (first boxed)))))
     "}\n"
     (apply str (for [h (vals (:hyps workspace))]
                  (format "\"%s\";\n" (id h))))
     (if (nil? acc) ""
         (format "\"%s\" [color=\"blue\", fontcolor=\"blue\", shape=\"box\"];"
                 (id acc)))
     (if (nil? acc) ""
         (let [implausible ((:implausible-fn acc) acc (vals (:hyps workspace)))]
           (if (empty? implausible) ""
               (if (< 0 (count implausible))
                 (apply str (concat
                             (map #(format "\"%s\" -> \"%s\" [arrowhead=\"box\"];\n"
                                           (id acc) (id %)) implausible)
                             (map #(format "\"%s\" [color=\"red\", fontcolor=\"red\"];\n"
                                           (id %))
                                  implausible)))
                 (format "\"%s\" -> \"%s\" [arrowhead=\"dot\", color=\"red\"];\n"
                         (id acc) (id (first implausible)))))))
     (if (nil? acc) ""
         (let [impossible ((:impossible-fn acc) acc (vals (:hyps workspace)))]
           (if (empty? impossible) ""
               (if (< 0 (count impossible))
                 (apply str (concat
                             (map #(format "\"%s\" -> \"%s\" [arrowhead=\"box\"];\n"
                                           (id acc) (id %)) impossible)
                             (map #(format "\"%s\" [color=\"red\", fontcolor=\"red\"];\n"
                                           (id %))
                                  impossible)))
                 (format "\"%s\" -> \"%s\" [arrowhead=\"box\"];\n"
                         (id acc) (id (first impossible)))))))
     "}\n")))

(defn find-best
  [workspace]
  (let [unexplained (lookup-hyps workspace (:unexplained workspace))
        candidates (lookup-hyps workspace (:candidates workspace))
        explainers (map #(find-explainers % candidates) unexplained)
        essentials (filter #(= 1 (count %)) explainers)]
    (if (not-empty essentials)
      ;; choose random most-confident essential
      (let [es (apply concat essentials)
            max-conf (:confidence (first (reverse (sort-by :confidence es))))
            acc (rand-nth (vec (filter #(= max-conf (:confidence %)) es)))]
        {:hyp acc :dot (dot-format workspace es acc)})

      ;; otherwise choose random most confident / most explanatory / hyp with
      ;; most of its "impossible" hyps already marked impossible
      (let [sorted (reverse (sort-by :confidence (apply concat explainers)))
            max-conf (:confidence (first sorted))
            most-conf (filter #(= max-conf (:confidence %)) sorted)
            expl-sorted (reverse (sort-by (comp count :explains) most-conf))
            max-expl (count (:explains (first expl-sorted)))
            most-expl (filter #(= max-expl (count (:explains %))) most-conf)
            count-imp (fn [h] (count (filter #(= IMPOSSIBLE (:confidence %))
                                             ((:impossible-fn h) h
                                              (vals (:hyps workspace))))))
            max-imp (apply max (conj (map count-imp most-expl) 0))
            most-imp (filter #(= max-imp (count-imp %)) most-expl)]
        (when (not-empty most-imp)
          (let [acc (rand-nth most-imp)]
            {:hyp acc :dot (dot-format workspace most-imp acc)}))))))

;;; TODO: add measure of how many iterations are used (for "compute" measure)

(defn explain
  [workspace]
  (let [ws (update-candidates-unexplained workspace)
        ws2 (reject-all-impossible ws)]
    (if (empty? (:unexplained ws2)) (update-in ws2 [:dot] conj (dot-format ws2 [] nil))
        (let [best (find-best ws2)]
          (if (nil? best) (update-in ws2 [:dot] conj (dot-format ws2 [] nil))
              (let [explains (filter #(% (:hyps workspace)) (:explains (:hyp best)))]
                (recur
                 (-> (update-in ws2 [:dot] conj (:dot best))
                     (add-abducer-log-msg
                      (conj explains (:id (:hyp best)))
                      (format "Accepting %s as explainer of %s." (name (:id (:hyp best)))
                              (apply str (interpose ", " (map name explains)))))
                     (accept-hyp (:hyp best))))))))))

