(ns retrospect.explore
  (:import (java.awt GridBagLayout Insets))
  (:import (javax.swing JSpinner SpinnerNumberModel))
  (:import (java.util.prefs Preferences))
  (:use [clojure.stacktrace])
  (:use [clj-swing.frame])
  (:use [clj-swing.label])
  (:use [clj-swing.panel])
  (:use [clj-swing.button])
  (:use [clj-swing.combo-box])
  (:use [clj-swing.text-field])
  (:use [retrospect.random :only [rgen new-seed my-rand-nth my-rand-int my-rand]])
  (:use [retrospect.simulate :only [get-default-params-ranges run-simulation init-ors]])
  (:use [retrospect.gui.results :only [update-results results-tab]])
  (:use [retrospect.epistemicstates :only [cur-ep]])
  (:use [retrospect.state]))

(def params-edit (ref ""))
(def metric-edit (ref ""))
(def log-box (ref ""))
(def min-max-selected (atom nil))
(def prefs (atom nil))
(def attempted (agent #{}))

(defn get-saved-params
  []
  (let [ps (try (read-string (.get @prefs (format "%s-%s-params"
                                                  (:name @reason) (:name @problem))
                                   (pr-str (get-default-params-ranges))))
                (catch Exception _ (get-default-params-ranges)))]
    (alter-var-root (var params) (constantly ps))))

(defn format-params
  [params]
  (str "{\n"
       (apply str (interpose "\n" (for [k (sort (keys params))]
                                    (format "%s %s" k (pr-str (params k))))))
       "\n}"))

(defn set-default-params
  []
  (let [ps (get-default-params-ranges)]
    (alter-var-root (var params) (constantly ps))
    (.put @prefs (format "%s-%s-params" (:name @reason) (:name @problem))
          (pr-str ps))
    (dosync (alter params-edit (constantly (format-params ps))))))

(def seed-spinner (JSpinner. (SpinnerNumberModel. 10 nil nil 1)))
(def repetitions-spinner (JSpinner. (SpinnerNumberModel. 10 nil nil 1)))
(def permutations-spinner (JSpinner. (SpinnerNumberModel. 20 nil nil 1)))
(def restarts-spinner (JSpinner. (SpinnerNumberModel. 5 nil nil 1)))

(defn get-spinner [spinner] (->> spinner .getModel .getNumber .intValue))
(defn set-spinner [spinner val] (. spinner setValue val))

(defn search
  [scorer permute state min-max permutations]
  (try
    (let [init state
          init-score (scorer state)
          comp (if (= "min" min-max) < >)
          opt (if (= "min" min-max) min max)]
      (loop [t 1
             state init
             score init-score
             best init
             best-score init-score]
        (if (> t permutations) {:best best :best-score best-score}
            (do
              (dosync (alter log-box str (format "\nPermutation %d of %d"
                                            t permutations)))
              (let [next (permute state)
                    next-score (scorer next)
                    better? (comp next-score score)]
                (when better?
                  (dosync (alter log-box str (format "\nNew best score: %.2f for %s"
                                                next-score next))))
                (if better?
                  (recur (inc t) next next-score
                         (if (comp next-score best-score) next best)
                         (opt next-score best-score))
                  (recur (inc t) state score best best-score)))))))
    (catch Exception e (print-stack-trace (root-cause e)))))

(defn run
  [metric params repetitions]
  (dosync (alter log-box str (format "\nSimulation %s" (pr-str params))))
  (let [rs (for [i (range repetitions)]
             (let [seed (my-rand-int 10000000)]
               (binding [rgen (new-seed seed)
                         last-id 0
                         retrospect.state/params (assoc params :Seed seed)]
                 (dosync (alter log-box str (format "\nSeed: %d" seed)))
                 (let [truedata ((:generate-truedata-fn @problem))
                       sensors ((:generate-sensors-fn @problem))
                       ors (init-ors sensors (:training truedata))
                       results (:results (cur-ep (:est (run-simulation truedata ors))))]
                   (dosync (alter retrospect.state/results conj (last results)))
                   (update-results)
                   (dosync (alter log-box str (format "\n%s = %s" (name metric)
                                                 (get (last results) metric))))
                   results))))
        avg (double (/ (reduce + (map #(get (last %) metric) rs)) repetitions))]
    (dosync (alter log-box str (format "\nAvg = %.2f" avg)))
    avg))

(defn next-params
  [def-ps last-params]
  (loop []
    (let [field (my-rand-nth (filter #(second (get def-ps %)) (keys def-ps)))
          val (my-rand-nth (get def-ps field))
          params (assoc last-params field val)]
      (if (or (= (get last-params field) val) (@attempted params))
        (recur)
        (do (dosync (alter log-box str (format "\nSwapping %s with %s" field val)))
            (send attempted conj params)
            params)))))

(defn random-params
  [params]
  (let [ps (reduce (fn [m k] (assoc m k (my-rand-nth (get params k)))) {} (keys params))]
    (if (@attempted ps) (recur params)
        (do (dosync (alter log-box str "\nRestarting...")) ps))))

(defn explore
  [state]
  (if (> (:restarts state) 0)
    (do
      (send *agent* explore)
      (binding [rgen (new-seed (my-rand-int 10000000))]
        (-> state
            (update-in [:restarts] dec)
            (update-in [:bests] conj (search #(run (:metric state) % (:repetitions state))
                                             (partial next-params (:params state))
                                             (random-params (:params state))
                                             (:min-max state) (:permutations state))))))
    state))

(comment (first (if (= "max" min-max) (reverse (sort-by :best-score (:bests state)))
                    (sort-by :best-score (:bests state)))))

(defn save-params-and-explore
  []
  (let [ps (read-string @params-edit)
        seed (get-spinner seed-spinner)
        permutations (get-spinner permutations-spinner)
        restarts (get-spinner restarts-spinner)
        repetitions (get-spinner repetitions-spinner)
        min-max @min-max-selected
        metric (keyword @metric-edit)]
    (alter-var-root (var params) (constantly ps))
    (.put @prefs (format "%s-%s-params" (:name @reason) (:name @problem))
          (pr-str (assoc ps :Seed seed :Permutations permutations
                         :Restarts restarts :Repetitions repetitions
                         :MinMax min-max :Metric (name metric))))
    (dosync (alter params-edit (constantly (format-params ps))))
    (when (and (not= "" min-max) (not= "" metric))
      (send (agent {:bests [] :params ps :seed seed :permutations permutations
                    :restarts restarts :repetitions repetitions
                    :min-max min-max :metric metric})
            explore))))

(defn mainframe
  []
  (frame :title "Explore"
         :layout (GridBagLayout.)
         :constrains (java.awt.GridBagConstraints.)
         :size [800 600]
         :show true
         :on-close :exit
         [:gridx 0 :gridy 0 :gridheight 9 :weightx 1.0 :weighty 1.0
          :fill :BOTH :insets (Insets. 5 5 5 5)
          _ (results-tab)

          :gridy 9 :gridheight 1 :weighty 0.0
          _ (scroll-panel (text-area :str-ref log-box :editable false :wrap true))
          
          :gridx 1 :gridy 0 :gridheight 1 :gridwidth 2 :weightx 0.05 :weighty 1.5
          _ (scroll-panel (text-area :str-ref params-edit :editable true
                                     :wrap false :rows 30))

          :gridx 1 :gridy 1 :weighty 0.0
          _ (button "Default params" :action ([_] (set-default-params)))

          :gridx 1 :gridy 2 :gridwidth 1
          _ (label "Min/max")
          :gridx 2
          _ (combo-box [] :model (seq-ref-combobox-model (ref ["min" "max"]) min-max-selected))

          :gridx 1 :gridy 3 :gridwidth 1
          _ (label "Metric")
          :gridx 2
          _ (text-field :str-ref metric-edit :editable true)

          :gridx 1 :gridy 4 :gridwidth 1
          _ (label "Repetitions")
          :gridx 2
          _ repetitions-spinner

          :gridx 1 :gridy 5 :gridwidth 1
          _ (label "Permutations")
          :gridx 2
          _ permutations-spinner

          :gridx 1 :gridy 6 :gridwidth 1
          _ (label "Restarts")
          :gridx 2
          _ restarts-spinner

          :gridx 1 :gridy 7 :gridwidth 1
          _ (label "Seed")
          :gridx 2
          _ seed-spinner
          
          :gridx 1 :gridy 8 :gridwidth 1
          _ (button "Explore" :action ([_] (save-params-and-explore)))
          :gridx 2
          _ (button "Stop" :action ([_]))

          :gridy 9 :weighty 1.0
          _ (panel)]))

(defn start-explore
  []
  (swap! prefs (constantly (.node (Preferences/userRoot)
                                  "/cc/artifice/retrospect/explore")))
  (get-saved-params)
  (set-spinner seed-spinner (or (:Seed params) 10))
  (set-spinner repetitions-spinner (or (:Repetitions params) 10))
  (set-spinner permutations-spinner (or (:Permutations params) 20))
  (set-spinner restarts-spinner (or (:Restarts params) 5))
  (swap! min-max-selected (constantly (or (:MinMax params) "min")))
  (dosync
   (alter metric-edit (constantly (or (:Metric params) "")))
   (alter params-edit (constantly (format-params (dissoc params :Seed :Repetitions
                                                         :Permutations :Restarts
                                                         :MinMax :Metric)))))
  
  (mainframe))
