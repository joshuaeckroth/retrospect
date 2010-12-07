(ns simulator.strategies.substrategies
  (:use [simulator.epistemicstates])
  (:use [simulator.hypotheses]))

(defn guess-type
  [ep-state type]
  (let [unexplained (unexplained-helper ep-state)]
    (if (not-empty unexplained)
     ;; choose an unexplained hyp and add a random explainer (if any exist)
     (let [hyp (choose-random-hyp unexplained)
           explainers (get-explainers (:hypspace ep-state) hyp)
           explainer (if (not-empty explainers)
                       (choose-random-hyp type ep-state explainers))]
       (if explainer
         (accept-explainer-type ep-state explainer hyp (name type))
         ep-state)))))

(defn guess
  [ep-state]
  (guess-type ep-state :guess))

(defn smartguess
  [ep-state]
  (guess-type ep-state :smartguess))

(defn essentials
  [ep-state]
  (let [essentials (find-essentials (:hypspace ep-state)
                                    (unexplained-helper ep-state))]
    (if (not-empty essentials)
      (let [{hyp :hyp explainer :essential} (choose-random-hyp essentials)]
        (accept-explainer-type ep-state explainer hyp "essential")))))

(defn best-threshold
  [threshold type ep-state]
  (let [best (find-best (:hypspace ep-state)
                        (unexplained-helper ep-state)
                        threshold type)]
    (if (not-empty best)
      (let [{hyp :hyp explainer :best chosen-type :type} (choose-random-hyp best)]
        (accept-explainer-type ep-state explainer hyp
                               (if (= type :smartbest)
                                 (format "%s-%d-%s" (name type) threshold chosen-type)
                                 (format "%s-%d" (name type) threshold)))))))

(defn best
  [threshold]
  (partial best-threshold threshold :best))

(defn smartbest
  [threshold]
  (partial best-threshold threshold :smartbest))
