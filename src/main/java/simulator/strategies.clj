(ns simulator.strategies
  (:require [simulator.types logs hypotheses])
  (:import [simulator.types.logs LogEntry AbducerLogEntry HypLogEntry])
  (:import [simulator.types.hypotheses HypothesisSpace])
  (:use simulator.types.hypotheses)
  (:use simulator.confidences)
  (:use clojure.set))

(defrecord StrategyState
    [strategy hypspace
     accepted considering                 ;; maps keyed by time
     hypothesized-at                      ;; maps keyed by time
     considering-before considering-afer  ;; maps keyed by time
     unexplained-before unexplained-after ;; maps keyed by time
     log abducer-log                      ;; maps keyed by time
     hyp-log                              ;; keyed by hyp
     resources problem-data])

(defn init-strat-states
  [strategies pdata]
  (for [s strategies]
    (StrategyState. s (init-hypspace)
                    {} {} {} {} {} {} {} {} {} {} ;; these are maps
                    {:compute 0 :milliseconds 0 :memory 0} pdata)))

(defn prepare-strat-state
  [strat-state time]
  (let [unexplained-before (find-unexplained
			    (:hypspace strat-state)
			    (get (:accepted strat-state) time))
	considering-before (get (:considering strat-state) time)]
    (-> strat-state
	(update-in [:unexplained-before] assoc time unexplained-before)
	(update-in [:considering-before] assoc time considering-before))))

(defn postprocess-strat-state
  [strat-state time startTime]
  (let [unexplained-after (find-unexplained
			   (:hypspace strat-state)
			   (get (:accepted strat-state) time))
	considering-after (get (:considering strat-state) time)
	milliseconds (+ (:milliseconds (:resources strat-state))
			(/ (- (. System (nanoTime)) startTime)
			   1000000.0))]
    (-> strat-state
	(update-in [:unexplained-after] assoc time unexplained-after)
	(update-in [:considering-after] assoc time considering-after)
	(update-in [:resources] assoc :milliseconds milliseconds))))

(defn add-log-msg
  [strat-state time msg]
  (let [entry (LogEntry. time msg)]
    (if (get (:log strat-state) time)
      (update-in strat-state [:log time] conj entry)
      (update-in strat-state [:log] assoc time [entry]))))

(defn add-abducer-log-msg
  [strat-state time hyps msg]
  (let [entry (AbducerLogEntry. time hyps msg)]
    (if (get (:abducer-log strat-state) time)
      (update-in strat-state [:abducer-log time] conj entry)
      (update-in strat-state [:abducer-log] assoc time [entry]))))

(defn add-hyp-log-msg
  [strat-state time hyp msg]
  (let [entry (HypLogEntry. time hyp msg)]
    (if (get (:hyp-log strat-state) hyp)
      (update-in strat-state [:hyp-log hyp] conj entry)
      (update-in strat-state [:hyp-log] assoc hyp [entry]))))

(defn add-hyp
  [strat-state time hyp explained log-msg]
  (let [hypspace (-> (:hypspace strat-state)
		     (update-in [:hyps] union #{hyp})
		     (add-explainers explained #{hyp})
		     (set-confidence hyp (get-apriori hyp)))]
    (-> strat-state
	(update-in [:hypothesized-at time] union #{hyp})
	(update-in [:considering time] union #{hyp})
	(assoc :hypspace hypspace)
	(add-log-msg time log-msg)
        (add-abducer-log-msg time #{hyp} "Adding hypothesis.")
        (add-hyp-log-msg time hyp "Adding hypothesis."))))

(defn penalize-conflicts
  [strat-state time conflicts log-msg]
  (if (empty? conflicts) strat-state
    (let [hypspace (reduce (fn [hs c] (penalize hs c))
                           (:hypspace strat-state) conflicts)
          ss (-> strat-state
                 (assoc :hypspace hypspace)
                 (add-abducer-log-msg time conflicts
                                      (format "Penalizing conflicts: %s."
                                              (get-hyp-ids-str conflicts))))]
      (reduce (fn [ss c] (add-hyp-log-msg ss time c log-msg))
              ss conflicts))))

(defn accept-hyp
  [strat-state time hyp]
  (let [conflicts (find-conflicts (:hypspace strat-state) #{hyp})]
    (-> strat-state
        (update-in [:accepted time] union #{hyp})
        (update-in [:considering time] difference #{hyp})
        (penalize-conflicts time conflicts
                            (format "Penalizing due to conflict with accepted hyp %s."
                                    (get-hyp-id-str hyp))))))

(defn accept-explainer-type
  [strat-state time explainer hyp type]
  (-> strat-state
      (accept-hyp time explainer)
      (add-abducer-log-msg time #{hyp explainer}
                           (format "Accepting %s %s as explainer of %s."
                                   type
                                   (get-hyp-id-str explainer)
                                   (get-hyp-id-str hyp)))
      (add-hyp-log-msg time explainer
                       (format "Accepting as %s explainer of %s."
                               type (get-hyp-id-str hyp)))
      (add-hyp-log-msg time hyp
                       (format "Hyp %s accepted as %s explainer."
                               (get-hyp-id-str explainer) type))))

(defn accept-guess
  [strat-state time explainer hyp]
  (accept-explainer-type strat-state time explainer hyp "guess"))

(defn accept-essential
  [strat-state time explainer hyp]
  (accept-explainer-type strat-state time explainer hyp "essential"))

(defn accept-best
  [strat-state time explainer hyp threshold]
  (accept-explainer-type strat-state time explainer hyp (format "best-by-%d" threshold)))

(defn force-acceptance
  [strat-state time hyp log-msg]
  (-> strat-state
      (accept-hyp time hyp)
      (add-abducer-log-msg time #{hyp}
                           (format "Forcing acceptance of: %s." (get-hyp-id-str hyp)))
      (add-hyp-log-msg time hyp "Forcing acceptance.")
      (add-log-msg time log-msg)))

(defn choose-random-hyp
  [hyps]
  (rand-nth (vec hyps)))

(defn unexplained-helper
  [strat-state time]
  (find-unexplained (:hypspace strat-state) (get (:accepted strat-state) time)))

(defn essentials-helper
  [strat-state time]
  (find-essentials (:hypspace strat-state)
                   (unexplained-helper strat-state time)))

(defn best-helper
  [strat-state time threshold]
  (find-best (:hypspace strat-state)
             (unexplained-helper strat-state time)
             threshold))

(defn guess
  [strat-state time]
  (let [unexplained (unexplained-helper strat-state time)]
    (if
     ;; choose an unexplained hyp and add a random explainer (if any exist)
     (not-empty unexplained)
     (let [hyp (choose-random-hyp unexplained)
           explainers (get-explainers (:hypspace strat-state) hyp)
           explainer (if (not-empty explainers) (choose-random-hyp explainers))]
       (if explainer
         (accept-guess strat-state time explainer hyp)
         strat-state)))))

(defn essentials
  [strat-state time]
  (let [essentials (essentials-helper strat-state time)]
    (if (not-empty essentials)
      (let [{hyp :hyp explainer :essential} (choose-random-hyp essentials)]
        (accept-essential strat-state time explainer hyp)))))

(defn best
  [threshold strat-state time]
  (let [best (best-helper strat-state time threshold)]
    (if (not-empty best)
      (let [{hyp :hyp explainer :best} (choose-random-hyp best)]
        (accept-best strat-state time explainer hyp threshold)))))

(def strategy-funcs
  {"guess" [guess]
   "es-guess" [essentials guess]
   "es-b1-guess" [essentials (partial best 1) guess]
   "es-b2-b1-guess" [essentials (partial best 2) (partial best 1) guess]
   "es-b3-b2-b1-guess"
   [essentials (partial best 3) (partial best 2) (partial best 1) guess]
   "es" [essentials]
   "es-b1" [essentials (partial best 1)]
   "es-b2-b1" [essentials (partial best 2) (partial best 1)]
   "es-b3-b2-b1" [essentials (partial best 3) (partial best 2) (partial best 1)]})

(def strategies (sort (keys strategy-funcs)))

(defn explain-recursive
  [strat-state time]
  (let [strat-funcs (get strategy-funcs (:strategy strat-state))]
    (loop [funcs strat-funcs
           ss strat-state]
      (if (empty? funcs) ss
          (let [ss2 ((first funcs) ss time)]
            (if ss2
              (recur funcs ss2)
              (recur (rest funcs) ss)))))))

(defn explain
  [strat-state time]
  (let [ss (prepare-strat-state strat-state time)
	startTime (. System (nanoTime))
        ss2 (explain-recursive strat-state time)]
    (postprocess-strat-state ss2 time startTime)))
