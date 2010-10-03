(ns simulator.strategies
  (:require [simulator.types logs hypotheses])
  (:import [simulator.types.logs LogEntry AbducerLogEntry HypLogEntry])
  (:import [simulator.types.hypotheses HypothesisSpace])
  (:use simulator.types.hypotheses)
  (:use simulator.confidences)
  (:use clojure.set))

(defrecord StrategyState
    [strategy hypspace
     accepted                             ;; map keyed by time
     hypothesized-at                      ;; maps keyed by time
     unexplained-before unexplained-after ;; maps keyed by time
     log abducer-log                      ;; maps keyed by time
     hyp-log                              ;; keyed by hyp
     resources problem-data])

(defn init-strat-states
  [strategies pdata]
  (doall (for [s strategies]
           (StrategyState. s (init-hypspace)
                           {} {} {} {} {} {} {} ;; these are maps
                           {:compute 0 :milliseconds 0 :memory 0} pdata))))

(defn prepare-strat-state
  [strat-state time]
  (let [unexplained-before (find-unexplained
			    (:hypspace strat-state)
			    (get (:accepted strat-state) time))]
    (-> strat-state
	(update-in [:unexplained-before] assoc time unexplained-before))))

(defn postprocess-strat-state
  [strat-state time startTime]
  (let [unexplained-after (find-unexplained
			   (:hypspace strat-state)
			   (get (:accepted strat-state) time))
	milliseconds (+ (:milliseconds (:resources strat-state))
			(/ (- (. System (nanoTime)) startTime)
			   1000000.0))]
    (-> strat-state
	(update-in [:unexplained-after] assoc time unexplained-after)
	(update-in [:resources] assoc :milliseconds milliseconds))))

(defn add-log-msg
  [strat-state time msg]
  (let [entry (LogEntry. time msg)]
    (if (get (:log strat-state) time)
      (update-in strat-state [:log time] conj entry)
      (update-in strat-state [:log] assoc time [entry]))))

(defn add-abducer-log-msg
  [strat-state time hyps msg]
  (let [entry (AbducerLogEntry. time (map get-hyp-id-str hyps) msg)]
    (if (get (:abducer-log strat-state) time)
      (update-in strat-state [:abducer-log time] conj entry)
      (update-in strat-state [:abducer-log] assoc time [entry]))))

(defn add-hyp-log-msg
  [strat-state time hyp msg]
  (let [entry (HypLogEntry. time (get-hyp-id-str hyp) msg)]
    (if (get (:hyp-log strat-state) hyp)
      (update-in strat-state [:hyp-log hyp] conj entry)
      (update-in strat-state [:hyp-log] assoc hyp [entry]))))

(defn add-hyp
  [strat-state time hyp explained log-msg]
  (let [hypspace (-> (:hypspace strat-state)
		     (update-in [:hyps] conj hyp)
		     (add-explainers explained [hyp])
		     (set-confidence hyp (get-apriori hyp)))]
    (-> strat-state
	(update-in [:hypothesized-at time] conj hyp)
	(assoc :hypspace hypspace)
	(add-log-msg time log-msg)
        (add-abducer-log-msg time [hyp] "Adding hypothesis.")
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
        (update-in [:accepted time] conj hyp)
        (penalize-conflicts time conflicts
                            (format "Penalizing due to conflict with accepted hyp %s."
                                    (get-hyp-id-str hyp))))))

(defn accept-explainer-type
  [strat-state time explainer hyp type]
  (-> strat-state
      (accept-hyp time explainer)
      (add-abducer-log-msg time [hyp explainer]
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

(defn force-acceptance
  [strat-state time hyp log-msg]
  (-> strat-state
      (accept-hyp time hyp)
      (add-abducer-log-msg time [hyp]
                           (format "Forcing acceptance of: %s." (get-hyp-id-str hyp)))
      (add-hyp-log-msg time hyp "Forcing acceptance.")
      (add-log-msg time log-msg)))

(defn choose-random-hyp
  ([hyps] (rand-nth (vec hyps)))
     
  ([type strat-state hyps]
     (cond (= :smartguess type)
           (let [hs (:hypspace strat-state)
                 threshold (first (sort (map (fn [h] (get-confidence hs h))
                                             hyps)))]
             (rand-nth (vec (filter (fn [h] (= threshold (get-confidence hs h))) hyps))))
           
           :else
           (choose-random-hyp hyps))))

(defn unexplained-helper
  [strat-state time]
  (find-unexplained (:hypspace strat-state) (get (:accepted strat-state) time)))

(defn guess-type
  [strat-state time type]
  (let [unexplained (unexplained-helper strat-state time)]
    (if (not-empty unexplained)
     ;; choose an unexplained hyp and add a random explainer (if any exist)
     (let [hyp (choose-random-hyp unexplained)
           explainers (get-explainers (:hypspace strat-state) hyp)
           explainer (if (not-empty explainers)
                       (choose-random-hyp type strat-state explainers))]
       (if explainer
         (accept-explainer-type strat-state time explainer hyp (name type))
         strat-state)))))

(defn guess
  [strat-state time]
  (guess-type strat-state time :guess))

(defn smartguess
  [strat-state time]
  (guess-type strat-state time :smartguess))

(defn essentials
  [strat-state time]
  (let [essentials (find-essentials (:hypspace strat-state)
                                    (unexplained-helper strat-state time))]
    (if (not-empty essentials)
      (let [{hyp :hyp explainer :essential} (choose-random-hyp essentials)]
        (accept-explainer-type strat-state time explainer hyp "essential")))))

(defn best-threshold
  [threshold type strat-state time]
  (let [best (find-best (:hypspace strat-state)
                        (unexplained-helper strat-state time)
                        threshold type)]
    (if (not-empty best)
      (let [{hyp :hyp explainer :best chosen-type :type} (choose-random-hyp best)]
        (accept-explainer-type strat-state time explainer hyp
                               (if (= type :smartbest)
                                 (format "%s-%d-%s" (name type) threshold chosen-type)
                                 (format "%s-%d" (name type) threshold)))))))

(defn best
  [threshold]
  (partial best-threshold threshold :best))

(defn smartbest
  [threshold]
  (partial best-threshold threshold :smartbest))

(def strategy-funcs
  {"guess" [guess]
   "smartguess" [smartguess]
   "es-guess" [essentials guess]
   "es-smartguess" [essentials smartguess]
   "es-b1-guess" [essentials (best 1) guess]
   "es-sb1-guess" [essentials (smartbest 1) guess]
   "es-b1-smartguess" [essentials (best 1) smartguess]
   "es-sb1-smartguess" [essentials (smartbest 1) smartguess]
   "es-b2-b1-guess" [essentials (best 2) (best 1) guess]
   "es-sb2-sb1-guess" [essentials (smartbest 2) (smartbest 1) guess]
   "es-b2-b1-smartguess" [essentials (best 2) (best 1) smartguess]
   "es-sb2-sb1-smartguess" [essentials (smartbest 2) (smartbest 1) smartguess]
   "es-b3-b2-b1-guess" [essentials (best 3) (best 2) (best 1) guess]
   "es-sb3-sb2-sb1-guess" [essentials (smartbest 3) (smartbest 2) (smartbest 1) guess]
   "es-b3-b2-b1-smartguess" [essentials (best 3) (best 2) (best 1) smartguess]
   "es-sb3-sb2-sb1-smartguess" [essentials (smartbest 3) (smartbest 2)
                                (smartbest 1) smartguess]
   "es-b4-b3-b2-b1-smartguess" [essentials (best 4) (best 3) (best 2) (best 1) smartguess]
   "es-sb4-sb3-sb2-sb1-smartguess" [essentials (smartbest 4) (smartbest 3) (smartbest 2)
                                (smartbest 1) smartguess]
   "es" [essentials]
   "es-b1" [essentials (best 1)]
   "es-sb1" [essentials (smartbest 1)]
   "es-b2-b1" [essentials (best 2) (best 1)]
   "es-sb2-sb1" [essentials (smartbest 2) (smartbest 1)]
   "es-b3-b2-b1" [essentials (best 3) (best 2) (best 1)]
   "es-sb3-sb2-sb1" [essentials (smartbest 3) (smartbest 2) (smartbest 1)]
   "es-sb4-sb3-sb2-sb1" [essentials (smartbest 4) (smartbest 3)
                         (smartbest 2) (smartbest 1)]})

(def strategies (sort (keys strategy-funcs)))

(defn explain-recursive
  [strat-state time]
  (let [strat-funcs (get strategy-funcs (:strategy strat-state))]
    (loop [funcs strat-funcs
           ss strat-state]
      (if (empty? funcs)
        (let [unexplained (unexplained-helper ss time)]
          (add-abducer-log-msg ss time unexplained
                               (format "%d unexplained hypotheses."
                                       (count unexplained))))
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
