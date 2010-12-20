(ns simulator.explain
  (:use [simulator.workspaces :only
         [unexplained-helper choose-random-hyp
          get-explainers accept-explainer-type
          find-essentials find-best]]))

(defn guess-type
  [workspace type]
  (let [unexplained (unexplained-helper workspace)]
    (if (not-empty unexplained)
     ;; choose an unexplained hyp and add a random explainer (if any exist)
     (let [hyp (choose-random-hyp unexplained)
           explainers (get-explainers workspace hyp)
           explainer (if (not-empty explainers)
                       (choose-random-hyp type workspace explainers))]
       (if explainer
         (accept-explainer-type workspace explainer hyp (name type))
         workspace)))))

(defn guess
  [workspace]
  (guess-type workspace :guess))

(defn smartguess
  [workspace]
  (guess-type workspace :smartguess))

(defn essentials
  [workspace]
  (let [essentials (find-essentials workspace)]
    (if (not-empty essentials)
      (let [{hyp :hyp explainer :essential} (choose-random-hyp essentials)]
        (accept-explainer-type workspace explainer hyp "essential")))))

(defn best-threshold
  [threshold type workspace]
  (let [best (find-best workspace threshold type)]
    (if (not-empty best)
      (let [{hyp :hyp explainer :best chosen-type :type} (choose-random-hyp best)]
        (accept-explainer-type workspace explainer hyp
                               (if (= type :smartbest)
                                 (format "%s-%d-%s" (name type) threshold chosen-type)
                                 (format "%s-%d" (name type) threshold)))))))

(defn best
  [threshold]
  (partial best-threshold threshold :best))

(defn smartbest
  [threshold]
  (partial best-threshold threshold :smartbest))

(defn explain-recursive
  [workspace funcs]
  (loop [fs funcs
         ws workspace]
    (if (empty? fs) ws
        (let [ws2 ((first fs) ws)]
          (if ws2
            (recur fs ws2)
            (recur (rest fs) ws))))))

(defn explain
  [workspace]
  (explain-recursive
   workspace
   [essentials (smartbest 4) (smartbest 3)
    (smartbest 2) (smartbest 1) smartguess]))