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
    (let [hypspace (reduce (fn [hs c]
                             (-> hs
                                 (penalize c)
                                 (penalize c)))
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

(defn accept-clearbest
  [strat-state time explainer hyp]
  (accept-explainer-type strat-state time explainer hyp "clearbest"))

(defn accept-weakbest
  [strat-state time explainer hyp]
  (accept-explainer-type strat-state time explainer hyp "weakbest"))

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

(defn clearbest-helper
  [strat-state time]
  (find-clearbest (:hypspace strat-state)
                  (unexplained-helper strat-state time)))

(defn weakbest-helper
  [strat-state time]
  (find-weakbest (:hypspace strat-state)
                 (unexplained-helper strat-state time)))

(defn explain-guess
  [strat-state time]
  (let [unexplained (unexplained-helper strat-state time)]
    (cond

     ;; don't continue 10% of the time
     (and (not-empty unexplained) (< (rand) 0.1))
     (add-abducer-log-msg strat-state time #{}
                          (format "Halting guess strategy. %d still unexplained."
                                  (count unexplained)))
    
     ;; choose an unexplained hyp and add a random explainer (if any exist)
     (not-empty unexplained)
     (let [hyp (choose-random-hyp unexplained)
           explainers (get-explainers (:hypspace strat-state) hyp)
           explainer (if (not-empty explainers) (choose-random-hyp explainers))]
       (if explainer
         (recur (accept-guess strat-state time explainer hyp) time)
         (recur strat-state time)))

     :else
     strat-state)))

(defn explain-essentials-guess
  [strat-state time]
  (let [essentials (essentials-helper strat-state time)]

    (cond

     (not-empty essentials)
     (let [{hyp :hyp explainer :essential} (choose-random-hyp essentials)]
       (recur (accept-essential strat-state time explainer hyp) time))
     
     :else
     (explain-guess strat-state time))))

(defn explain-essentials-clearbest-guess
  [strat-state time]
  (let [essentials (essentials-helper strat-state time)
        clearbest (clearbest-helper strat-state time)]

    (cond

     (not-empty essentials)
     (let [{hyp :hyp explainer :essential} (choose-random-hyp essentials)]
       (recur (accept-essential strat-state time explainer hyp) time))

     (not-empty clearbest)
     (let [{hyp :hyp explainer :clearbest} (choose-random-hyp clearbest)]
       (recur (accept-clearbest strat-state time explainer hyp) time))

     :else
     (explain-guess strat-state time))))

(defn explain-essentials-clearbest-weakbest-guess
  [strat-state time]
  (let [essentials (essentials-helper strat-state time)
        clearbest (clearbest-helper strat-state time)
        weakbest (weakbest-helper strat-state time)]

    (cond

     (not-empty essentials)
     (let [{hyp :hyp explainer :essential} (choose-random-hyp essentials)]
       (recur (accept-essential strat-state time explainer hyp) time))

     (not-empty clearbest)
     (let [{hyp :hyp explainer :clearbest} (choose-random-hyp clearbest)]
       (recur (accept-clearbest strat-state time explainer hyp) time))

     (not-empty weakbest)
     (let [{hyp :hyp explainer :weakbest} (choose-random-hyp weakbest)]
       (recur (accept-weakbest strat-state time explainer hyp) time))

     :else
     (explain-guess strat-state time))))

(defn explain
  [strat-state time]
  (let [ss (prepare-strat-state strat-state time)
	startTime (. System (nanoTime))
	ss2
	(case (:strategy ss)
	      "guess" (explain-guess ss time)
	      "es-guess" (explain-essentials-guess ss time)
	      "es-cb-guess" (explain-essentials-clearbest-guess ss time)
	      "es-cb-wb-guess"
	      (explain-essentials-clearbest-weakbest-guess ss time))]
    (postprocess-strat-state ss2 time startTime)))

(def strategies ["guess" "es-guess" "es-cb-guess" "es-cb-wb-guess"])

