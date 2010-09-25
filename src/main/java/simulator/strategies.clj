(ns simulator.strategies
  (:require [simulator.types logs hypotheses])
  (:import [simulator.types.logs LogEntry AbducerLogEntry])
  (:import [simulator.types.hypotheses HypothesisSpace])
  (:use simulator.types.hypotheses)
  (:use clojure.set))

(defrecord StrategyState
    [strategy hypspace
     accepted considering rejected        ;; maps keyed by time
     hypothesized                         ;; maps keyed by time
     considering-before considering-afer  ;; maps keyed by time
     unexplained-before unexplained-after ;; maps keyed by time
     log abducer-log 
     resources problem-data])

(defn init-strat-state
  [strategy pdata]
  (StrategyState. strategy
		  (init-hypspace)
		  {} {} {} {} {} {} {} {} {} {} ;; these are maps, keyed by time
		  {:compute 0 :milliseconds 0 :memory 0} pdata))

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
	(add-log-msg time log-msg))))

(defn force-acceptance
  [strat-state time hyp log-msg]
  (-> strat-state
      (update-in [:accepted time] union #{hyp})
      (update-in [:considering time] difference #{hyp})
      (add-abducer-log-msg time #{hyp}
                           (format "Forcing acceptance of: %s" (str hyp)))
      (add-log-msg time log-msg)))

(defn format-logs
  [strat-state]
  (apply str (map str (:log strat-state))))

(defn conflicts-helper
  [strat-state time]
  (difference (find-conflicts (:hypspace strat-state) (get (:accepted strat-state) time))
	      (get (:rejected strat-state) time)))

(defn unexplained-helper
  [strat-state time]
  (find-unexplained (:hypspace strat-state) (get (:accepted strat-state) time)))

(defn essentials-helper
  [strat-state unexplained]
  (find-essentials (:hypspace strat-state) unexplained))

(defn clearbest-helper
  [strat-state unexplained]
  (find-clearbest (:hypspace strat-state) unexplained))

(defn weakbest-helper
  [strat-state unexplained]
  (find-weakbest (:hypspace strat-state) unexplained))

(defn reject-conflicts-helper
  [strat-state time conflicts]
  (-> strat-state
      (update-in [:rejected time] union conflicts)
      (update-in [:considering time] difference conflicts)
      (add-abducer-log-msg time conflicts
			   (format "Rejecting conflicts: %s"
				   (apply str (interpose "," (map :id conflicts)))))))

(defn accept-essentials-helper
  [strat-state time essentials]
  (-> strat-state
      (update-in [:accepted time] union essentials)
      (update-in [:considering time] difference essentials)
      (add-abducer-log-msg time essentials
			   (format "Accepting essentials: %s"
				   (apply str (interpose "," (map :id essentials)))))))

(defn accept-clearbest-helper
  [strat-state time clearbest]
  (if (empty? clearbest) strat-state
      (let [hyp (:hyp (first clearbest))
	    explainer (:explainer (first clearbest))]
	(recur (-> strat-state
		   (update-in [:accepted time] union #{explainer})
		   (update-in [:considering time] difference #{explainer})
		   (add-abducer-log-msg time #{explainer}
					(format "Accepting clearbest %s" (:id explainer))))
	       time
	       (rest clearbest)))))

(defn accept-weakbest-helper
  [strat-state time weakbest]
  (if (empty? weakbest) strat-state
      (let [hyp (:hyp (first weakbest))
	    explainer (:explainer (first weakbest))]
	(recur (-> strat-state
		   (update-in [:accepted time] union #{explainer})
		   (update-in [:considering time] difference #{explainer})
		   (add-abducer-log-msg time #{explainer}
					(format "Accepting weakbest %s" (:id explainer))))
	       time
	       (rest weakbest)))))

(defn explain-guess
  [strat-state time]
  (let [conflicts (conflicts-helper strat-state time)
	unexplained (unexplained-helper strat-state time)]
    (cond
     
     ;; reject any conflicts
     (not-empty conflicts)
     (recur (reject-conflicts-helper strat-state time conflicts) time)
     
     ;; don't continue 10% of the time
     (< (rand) 0.1) strat-state

     ;; choose an unexplained hyp and add a random explainer (if any exist)
     (not-empty unexplained)
     (let [hyp (rand-nth (vec unexplained))
	   explainers (difference (get-explainers (:hypspace strat-state) hyp)
				  (get (:rejected strat-state) time))]
       (if (empty? explainers)

	 ;; no explainers, just add the hyp
	 (recur (-> strat-state
		    (update-in [:accepted time] union #{hyp})) time)

	 ;; some explainers, add a random one and the hyp
	 (let [expl (rand-nth (vec explainers))]
	   (recur (-> strat-state
		      ;; TODO: does not support composite explainers
		      (update-in [:accepted time] union #{hyp expl})
		      (update-in [:considering time] difference #{hyp expl})
		      (add-abducer-log-msg time #{hyp expl}
					   (format "Accepting guess that %s explains %s"
						   (:id expl) (:id hyp))))
		  time))))
     
     :else strat-state)))

(defn explain-essentials-guess
  [strat-state time]
  (let [conflicts (conflicts-helper strat-state time)
	unexplained (unexplained-helper strat-state time)
	essentials (essentials-helper strat-state unexplained)]
    (cond
     
     ;; reject any conflicts
     (not-empty conflicts)
     (recur (reject-conflicts-helper strat-state time conflicts) time)

     ;; accept essentials
     (not-empty essentials)
     (recur (accept-essentials-helper strat-state time essentials) time)

     ;; no more essentials, so refer to explain-guess for the rest
     :else (explain-guess strat-state time))))

(defn explain-essentials-clearbest-guess
  [strat-state time]
  (let [conflicts (conflicts-helper strat-state time)
	unexplained (unexplained-helper strat-state time)
	essentials (essentials-helper strat-state unexplained)
	clearbest (clearbest-helper strat-state unexplained)]
    (cond

     ;; reject any conflicts
     (not-empty conflicts)
     (recur (reject-conflicts-helper strat-state time conflicts) time)

     ;; accept essentials
     (not-empty essentials)
     (recur (accept-essentials-helper strat-state time essentials) time)

     ;; accept clearbest
     (not-empty clearbest)
     (recur (accept-clearbest-helper strat-state time clearbest) time)

     ;; no more clearbest, so refer to explain-guess for the rest
     :else (explain-guess strat-state time))))

(defn explain-essentials-clearbest-weakbest-guess
  [strat-state time]
  (let [conflicts (conflicts-helper strat-state time)
	unexplained (unexplained-helper strat-state time)
	essentials (essentials-helper strat-state unexplained)
	clearbest (clearbest-helper strat-state unexplained)
	weakbest (weakbest-helper strat-state unexplained)]
    (cond

     ;; reject any conflicts
     (not-empty conflicts)
     (recur (reject-conflicts-helper strat-state time conflicts) time)

     ;; accept essentials
     (not-empty essentials)
     (recur (accept-essentials-helper strat-state time essentials) time)

     ;; accept clearbest
     (not-empty clearbest)
     (recur (accept-clearbest-helper strat-state time clearbest) time)

     ;; accept weakbest
     (not-empty weakbest)
     (recur (accept-weakbest-helper strat-state time weakbest) time)

     ;; no more weakbest, so refer to explain-guess for the rest
     :else (explain-guess strat-state time))))

(defn explain
  [strat-state time]
  (let [ss (prepare-strat-state strat-state time)
	startTime (. System (nanoTime))
	ss2
	(case (:strategy ss)
	      "guess" (explain-guess ss time)
	      "essentials-guess" (explain-essentials-guess ss time)
	      "essentials-clearbest-guess" (explain-essentials-clearbest-guess ss time)
	      "essentials-clearbest-weakbest-guess"
	      (explain-essentials-clearbest-weakbest-guess ss time))]
    (postprocess-strat-state ss2 time startTime)))

(def strategies ["guess" "essentials-guess" "essentials-clearbest-guess"
		 "essentials-clearbest-weakbest-guess"])

