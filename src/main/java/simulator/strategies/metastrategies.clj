(ns simulator.strategies.metastrategies
  (:require [simulator logs])
  (:import [simulator.logs MetaLogEntry])
  (:use [simulator.confidences])
  (:use [simulator.epistemicstates]))

(defn add-meta-log-msg
  [or-state ep-state ep-state-revisit msg]
  (let [entry (MetaLogEntry. ep-state ep-state-revisit msg)]
    (update-in or-state [:meta-log] conj entry)))

(defn prepare-meta-abduction
  [or-state ep-state]
  (let [branched-ep-state-tree (new-branch-ep-state (:ep-state-tree or-state)
                                                    (:ep-state or-state) ep-state)]
    (-> or-state
        (assoc :ep-state-tree branched-ep-state-tree)
        (assoc :ep-state (current-ep-state branched-ep-state-tree)))))

(defn least-conf-recent
  [or-state]
  (if-let [least-conf (find-least-confident-decision (:ep-state-tree or-state))]
    (let [this-conf (measure-decision-confidence (:ep-state or-state))]
      ;; is the present decision confidence worse than NEUTRAL
      ;; and the least confident past decision worse than NEUTRAL
      ;; and the number of existing branches not too large?
      (if (and (> NEUTRAL this-conf)
               (> NEUTRAL (:confidence (:decision least-conf)))
               (> 5 (count-branches (:ep-state-tree or-state) least-conf)))

        ;; Note that presently no check is made for how low conf is
        ;; least-conf; perhaps least-conf has high confidence... what
        ;; would this imply?
        (-> or-state
            (add-meta-log-msg
             (:ep-state or-state) least-conf
             (format "Confidence of %s is %s, found 'least confident'
                      prior %s which has fewer than 5 branches."
                     (str (:ep-state or-state)) (confidence-str this-conf)
                     (str least-conf)))
            (prepare-meta-abduction least-conf))))))

(def meta-strategy-funcs
  {"none" (fn [_] nil)
   "least-conf-recent" least-conf-recent})

(def meta-strategies (sort (keys meta-strategy-funcs)))