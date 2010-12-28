(ns simulator.problems.peyer.case
  (:require [simulator hypotheses])
  (:require [simulator.strategies core])
  (:require [clojure string])
  (:import [simulator.hypotheses HypothesisSpace])
  (:import [simulator.strategies StrategyState])
  (:use clojure.set)
  (:use clojure.test)
  (:use simulator.strategies)
  (:use simulator.hypotheses))

(defrecord Hypothesis [id explained-by conflicts string]
  Object
  (toString [_] (str id ": " (clojure.string/replace string #"\s+" " ")
		     \newline \tab "Explainers: "
		     (apply str (interpose ", " explained-by))
		     \newline \tab "Conflicts:  "
		     (apply str (interpose ", " conflicts)))))

(def *observations*
  (set (map (fn [{hyp :hyp explained-by :explained-by
		  conflicts :conflicts string :str}]
	      (Hypothesis. hyp explained-by conflicts string))
	    #{{:hyp "E1" :explained-by #{"I1" "G1"}
	       :str "Knott's body and car were found on a frontage road near
                     Interstate-15."}
	      {:hyp "E2" :explained-by #{"G6"}
	       :str "22 young women reported being talked to at length by
                     Peyer after being stopped near where Knott's body was found."}
	      {:hyp "E3" :explained-by #{"G4"}
	       :str "Calderwood said that he saw a patrol care pull over a Volkswagen
                     like Knott's near Interstate-15."}
	      {:hyp "E4" :explained-by #{"G5" "I2"}
	       :str "Calderwood came forward only after the trial."}
	      {:hyp "E5" :explained-by #{"I2"}
	       :str "Calderwood changed his story several times."}
	      {:hyp "E6" :explained-by #{"G3" "I3"}
	       :str "6 fibers found on Knott's body matched Peyer's uniform."}
	      {:hyp "E7" :explained-by #{"G1" "I4"}
	       :str "Ogilvie said Peyer quizzed her about the case and acted strangely."}
	      {:hyp "E8" :explained-by #{"I4a"}
	       :str "Dotson said Ogilvie is a liar."}
	      {:hyp "E9" :explained-by #{"G2" "I5"}
	       :str "Anderson and Schwartz saw scratches on Peyer's face the night
                     of the killing."}
	      {:hyp "E10" :explained-by #{"G1" "I6"}
	       :str "Martin said she saw Peyer pull Knott's Volkswagen over."}
	      {:hyp "E11" :explained-by #{"I6"}
	       :str "Martin came forward only just before the trial."}
	      {:hyp "E12" :explained-by #{"G7" "I7"}
	       :str "Anderson said that she saw Peyer wipe off his nightstick
                     in his trunk."}
	      {:hyp "E13" :explained-by #{"G8" "I7"}
	       :str "Anderson did not say anything about the nightstick when she was
                        first interrogated."}
	      {:hyp "E14" :explained-by #{"G1" #{"I1" "E15"}}
	       :str "Bloodstains found on Knott's clothes matched Peyer's blood."}
	      {:hyp "E15"
	       :str "12,800 other Sand Diegans had blood matching that on
                     Knott's clothes."}
	      {:hyp "E16" :explained-by #{"I1"}
	       :str "A shabby hitchhiker was lunging at cars near the Interstate-15
                     entrance."}
	      {:hyp "E17" :explained-by #{"I8"}
	       :str "Peyer had a spotless record with the California Highway Patrol."}})))

(def *hypotheses*
  (set (map (fn [{hyp :hyp explained-by :explained-by
		  conflicts :conflicts string :str}]
	      (Hypothesis. hyp explained-by conflicts string))
	    #{{:hyp "G1" :str "Peyer killed Knott."
	       :conflicts #{"I1" "I8"}}
	      {:hyp "G2" :str "Knott scratchd Peyer's face."
	       :conflicts #{"I5"}}
	      {:hyp "G3" :str "Fibers from Peyer's uniform were transferred to Knott."
	       :conflicts #{"I3"}}
	      {:hyp "G4" :str "Peyer pulled Knott over."}
	      {:hyp "G5" :str "Calderwood was reluctant to come forward because
                               he wanted to protect his family from publicity."
	       :conflicts #{"I2"}}
	      {:hyp "G6" :str "Peyer liked to pull over young women."}
	      {:hyp "G7" :str "Peyer had a bloody nightstick."
	       :conflicts #{"I7"}}
	      {:hyp "G8" :str "Anderson was having personal problems when she
                               was first interrogated."}
	      {:hyp "I1" :str "Someone other than Peyer killed Knott."
	       :conflicts #{"G1"}}
	      {:hyp "I2" :str "Calderwood made his story up."
	       :conflicts #{"G5"}}
	      {:hyp "I3" :str "The 6 fibers floated around in the police evidence room."
	       :conflicts #{"G3"}}
	      {:hyp "I4" :explained-by #{"I4a"} :str "Ogilvie lied."}
	      {:hyp "I4a" :str "Ogilvie is a liar."}
	      {:hyp "I5" :str "Peyer's scratches came from a fence."
	       :conflicts #{"G2"}}
	      {:hyp "I6" :str "Martin lied."}
	      {:hyp "I7" :str "Anderson was mistaken about the nightstick."
	       :conflicts #{"G7"}}
	      {:hyp "I8" :str "Peyer is a good man."
	       :conflicts #{"G1"}}})))

(defn print-hyps
  [hyps]
  (doseq [h (sort-by :id hyps)] (println (str h))))

(defn lookup-hyps
  [strs]
  (set (filter (fn [hyp] (some #(= (:id hyp) %) strs))
	       (union *observations* *hypotheses*))))

(defn lookup-hyp
  [str]
  (first (lookup-hyps [str])))

(defn build-explainers
  [hypspace hyps]
  (if (empty? hyps) hypspace
      (let [hyp (first hyps)
	    explainers (lookup-hyps (:explained-by hyp))]
	(recur (update-in hypspace [:explainers] assoc hyp explainers)
	       (rest hyps)))))

(defn build-conflicts
  [hypspace hyps]
  (if (empty? hyps) hypspace
      (let [hyp (first hyps)
	    conflicts (lookup-hyps (:conflicts hyp))]
	(recur (update-in hypspace [:conflicts] assoc hyp conflicts)
	       (rest hyps)))))

(defn build-hypspace
  [hyps]
  (-> (init-hypspace)
      (build-explainers hyps)
      (build-conflicts hyps)))

(defn peyer-hypspace
  []
  (build-hypspace (union *observations* *hypotheses*)))

(defn peyer-strat-state
  []
  (StrategyState. nil (peyer-hypspace) *observations* {} {} {} {} {} {} {} nil))

