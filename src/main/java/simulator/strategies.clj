(ns simulator.strategies
  (:require [simulator.types logs hypotheses])
  (:import [simulator.types.logs LogEntry])
  (:import [simulator.types.hypotheses HypothesisSpace])
  (:use simulator.types.hypotheses)
  (:use clojure.set))

(defrecord StrategyState
    [strategy hypspace accepted considering rejected
     log abducer-log resources problem-data])

(defn init-strat-state
  [strategy pdata]
  (StrategyState. strategy
		  (init-hypspace)
		  #{} #{} #{} ;; these are sets
		  [] [] {} pdata))

(defn add-log-msg
  [strat-state time msg]
  (update-in strat-state [:log] conj (LogEntry. time msg)))

(defn add-abducer-log-msg
  [strat-state msg]
  (update-in strat-state [:abducer-log] conj msg))

(defn format-logs
  [strat-state]
  (apply str (map str (:log strat-state))))

(defn conflicts-helper
  [strat-state]
  (difference (find-conflicts (:hypspace strat-state) (:accepted strat-state))
	      (:rejected strat-state)))

(defn unexplained-helper
  [strat-state]
  (find-unexplained (:hypspace strat-state) (:accepted strat-state)))

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
  [strat-state conflicts]
  (-> strat-state
      (update-in [:rejected] union conflicts)
      (update-in [:considering] difference conflicts)
      (add-abducer-log-msg (str "Rejecting conflicts: " (apply str conflicts)))))

(defn accept-essentials-helper
  [strat-state essentials]
  (-> strat-state
      (update-in [:accepted] union essentials)
      (update-in [:considering] difference essentials)
      (add-abducer-log-msg (str "Accepting essentials: " (apply str essentials)))))

(defn accept-clearbest-helper
  [strat-state clearbest]
  (if (empty? clearbest) strat-state
      (let [hyp (:hyp (first clearbest))
	    explainer (:explainer (first clearbest))]
	(recur (-> strat-state
		   (update-in [:accepted] conj explainer)
		   (update-in [:considering] disj explainer)
		   (add-abducer-log-msg (str "Accepting clearbest " explainer)))
	       (rest clearbest)))))

(defn accept-weakbest-helper
  [strat-state weakbest]
  (if (empty? weakbest) strat-state
      (let [hyp (:hyp (first weakbest))
	    explainer (:explainer (first weakbest))]
	(recur (-> strat-state
		   (update-in [:accepted] conj explainer)
		   (update-in [:considering] disj explainer)
		   (add-abducer-log-msg (str "Accepting weakbest " explainer)))
	       (rest weakbest)))))

(defn explain-guess
  [strat-state]
  (let [conflicts (conflicts-helper strat-state)
	unexplained (unexplained-helper strat-state)]
    (cond
     
     ;; reject any conflicts
     (not-empty conflicts)
     (recur (reject-conflicts-helper strat-state conflicts))
     
     ;; don't continue 10% of the time
     (< (rand) 0.1) strat-state

     ;; choose an unexplained hyp and add a random explainer (if any exist)
     (not-empty unexplained)
     (let [hyp (rand-nth (vec unexplained))
	   explainers (difference (get-explainers (:hypspace strat-state) hyp)
				  (:rejected strat-state))]
       (if (empty? explainers)

	 ;; no explainers, just add the hyp
	 (recur (-> strat-state
		    (update-in [:accepted] conj hyp)))

	 ;; some explainers, add a random one and the hyp
	 (let [expl (rand-nth (vec explainers))]
	   (recur (-> strat-state
		      ;; TODO: does not support composite explainers
		      (update-in [:accepted] union #{hyp expl})
		      (update-in [:considering] difference #{hyp expl}))))))
     
     :else strat-state)))

(defn explain-essentials-guess
  [strat-state]
  (let [conflicts (conflicts-helper strat-state)
	unexplained (unexplained-helper strat-state)
	essentials (essentials-helper strat-state unexplained)]
    (cond
     
     ;; reject any conflicts
     (not-empty conflicts)
     (recur (reject-conflicts-helper strat-state conflicts))

     ;; accept essentials
     (not-empty essentials)
     (recur (accept-essentials-helper strat-state essentials))

     ;; no more essentials, so refer to explain-guess for the rest
     :else (explain-guess strat-state))))

(defn explain-essentials-clearbest-guess
  [strat-state]
  (let [conflicts (conflicts-helper strat-state)
	unexplained (unexplained-helper strat-state)
	essentials (essentials-helper strat-state unexplained)
	clearbest (clearbest-helper strat-state unexplained)]
    (cond

     ;; reject any conflicts
     (not-empty conflicts)
     (recur (reject-conflicts-helper strat-state conflicts))

     ;; accept essentials
     (not-empty essentials)
     (recur (accept-essentials-helper strat-state essentials))

     ;; accept clearbest
     (not-empty clearbest)
     (recur (accept-clearbest-helper strat-state clearbest))

     ;; no more clearbest, so refer to explain-guess for the rest
     :else (explain-guess strat-state))))

(defn explain-essentials-clearbest-weakbest-guess
  [strat-state]
  (let [conflicts (conflicts-helper strat-state)
	unexplained (unexplained-helper strat-state)
	essentials (essentials-helper strat-state unexplained)
	clearbest (clearbest-helper strat-state unexplained)
	weakbest (weakbest-helper strat-state unexplained)]
    (cond

     ;; reject any conflicts
     (not-empty conflicts)
     (recur (reject-conflicts-helper strat-state conflicts))

     ;; accept essentials
     (not-empty essentials)
     (recur (accept-essentials-helper strat-state essentials))

     ;; accept clearbest
     (not-empty clearbest)
     (recur (accept-clearbest-helper strat-state clearbest))

     ;; accept weakbest
     (not-empty weakbest)
     (recur (accept-weakbest-helper strat-state weakbest))

     ;; no more weakbest, so refer to explain-guess for the rest
     :else (explain-guess strat-state))))

(defn explain
  [strat-state]
  (case (:strategy strat-state)
	"guess" (explain-guess strat-state)
	"essentials-guess" (explain-essentials-guess strat-state)
	"essentials-clearbest-guess" (explain-essentials-clearbest-guess strat-state)
	"essentials-clearbest-weakbest-guess"
	(explain-essentials-clearbest-weakbest-guess strat-state)))

(def strategies ["guess" "essentials-guess" "essentials-clearbest-guess"
		 "essentials-clearbest-weakbest-guess"])

