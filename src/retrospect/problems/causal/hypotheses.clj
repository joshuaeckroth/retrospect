(ns retrospect.problems.causal.hypotheses
  (:use [loom.graph :only [transpose neighbors incoming]])
  (:use [loom.alg :only [pre-traverse]])
  (:use [loom.attr :only [attr]])
  (:use [retrospect.epistemicstates :only [add-fact add-hyp]])
  (:use [retrospect.workspaces :only [new-hyp]])
  (:use [retrospect.sensors :only [sensed-at]]))

(def compute 0)
(def memory 0)

(defn hypothesize
  [ep-state sensors time-now]
  (binding [compute 0 memory 0]
    (let [{:keys [network believed]} (:problem-data ep-state)
          observed (filter not-empty (mapcat (fn [s] (map (fn [t] (sensed-at s t))
                                                          (range 0 (inc time-now))))
                                             sensors))
          observed-unexplained (filter (fn [[n val]]
                                         (let [bel-expls (filter #(get believed %)
                                                                 (incoming network n))]
                                           (or
                                            (empty? bel-expls)
                                            (and (= val :on)
                                                 (not-any? #(= :on (:value (get believed %)))
                                                           bel-expls))
                                            (and (= val :off)
                                                 (every? #(= :on (:value (get believed %)))
                                                         bel-expls)))))
                                       observed)
          observed-hyps (reduce (fn [m [n val]]
                                  (assoc m n
                                         (new-hyp "Obs" :sensor nil
                                                  1.0 :or [] []
                                                  (format "%s observed %s" n (name val))
                                                  {:node n :value val})))
                                {} (filter not-empty observed-unexplained))
          network-trans (transpose network)
          implicated (filter #(not (get believed %))
                             (mapcat #(rest (pre-traverse network-trans %))
                                     (map first observed)))
          hyps (reduce (fn [m node]
                         (assoc m node
                                {:on (new-hyp "On" :node node
                                              (attr network node :apriori)
                                              :or [] []
                                              (format "%s is on" node)
                                              {:node node :value :on})
                                 :off (new-hyp "Off" :node node
                                               (- 1.0 (attr network node :apriori))
                                               :or [] []
                                               (format "%s is off" node)
                                               {:node node :value :off})}))
                       {} implicated)
          extract-explains (fn [hyps node val]
                             (filter identity
                                     (map (fn [n]
                                            (if-let [obs (get observed-hyps n)]
                                              (let [obs-val (:value (:data obs))]
                                                (if (= obs-val val)
                                                  obs))
                                              (get (get hyps n) val)))
                                          (neighbors network node))))
          hyps-explains (reduce (fn [m node]
                                  (reduce (fn [m2 val]
                                            (assoc-in m2 [node val :explains]
                                                      (extract-explains m2 node val)))
                                          m [:on :off]))
                                hyps implicated)]
      [(reduce (fn [ep hyp] (add-hyp ep hyp))
               (reduce (fn [ep hyp] (add-fact ep hyp))
                       ep-state (vals observed-hyps))
               (mapcat (fn [val] (map (fn [n] (get (get hyps-explains n) val)) implicated))
                       [:on :off]))
       {:compute compute :memory memory}])))

(defn commit-decision
  [pdata accepted rejected unaccepted time-now]
  (let [believed (reduce (fn [b h] (assoc b (:node (:data h))
                                          {:value (:value (:data h)) :hyp h}))
                         (:believed pdata) accepted)]
    (assoc pdata :believed believed)))

(defn retract
  [pdata hyp]
  pdata)

(defn no-explainer-hyps
  [hyps pdata]
  [])
