(ns retrospect.problems.causal.hypotheses
  (:use [loom.graph :only [transpose neighbors incoming]])
  (:use [loom.alg :only [pre-traverse]])
  (:use [loom.attr :only [attr]])
  (require [clojure.set :as set])
  (:use [retrospect.problems.causal.javabayes :only
         [build-bayesnet observe-seq unobserve-all observe get-posterior-marginal
          get-explanation]])
  (:use [retrospect.epistemicstates :only [add-fact add-hyp]])
  (:use [retrospect.workspaces :only [new-hyp]])
  (:use [retrospect.sensors :only [sensed-at]]))

(def compute 0)
(def memory 0)

(defn explanatory-delta
  "Is P(node2=val2|node1=val2) > P(node2=val2)? If so, node1=val1 is
   explanatory (according to Gardenfors)."
  [bn node1 val1 node2 val2 observed]
  (unobserve-all bn)
  (observe-seq bn (filter #(not= node2 (first %)) observed))
  (let [prior (get-posterior-marginal bn node2 val2)]
    (observe bn node1 val1)
    (let [posterior (get-posterior-marginal bn node2 val2)]
      (- posterior prior))))

(defn find-explanatory-assignments
  [bn network implicated observed]
  (filter #(< 0 (:delta %))
          (map (fn [[[n1 v1] [n2 v2]]]
                 {:node1 n1 :val1 v1
                  :node2 n2 :val2 v2
                  :delta (explanatory-delta bn n1 v1 n2 v2 observed)})
               (mapcat (fn [n]
                         (let [cs (neighbors network n)
                               cs-no-obs (set/difference cs (set (map first observed)))
                               cs-vals (mapcat (fn [c] (map (fn [v] [c v])
                                                            (attr network c :values)))
                                               cs-no-obs)
                               cs-obs-vals (concat cs-vals (filter #(cs (first %)) observed))
                               vals (attr network n :values)]
                           (mapcat (fn [nv] (map (fn [cv] [nv cv]) cs-obs-vals))
                                   (map (fn [v] [n v]) vals))))
                       implicated))))

(defn make-explanation-hyps
  [explanatory observed-hyps]
  (let [get-hyps (fn [m] (map (fn [k] (get m k))
                              (map (fn [{:keys [node1 val1 node2 val2]}]
                                     [node1 val1 node2 val2])
                                   explanatory)))
        hyps-no-explains
        (reduce (fn [m {:keys [node1 val1 node2 val2 delta]}]
                  (let [h (new-hyp "Expl" :explanation node1 delta :and [] []
                                   (format "%s=%s explains %s=%s" node1 val1 node2 val2)
                                   {:node1 node1 :val1 val1
                                    :node2 node2 :val2 val2})]
                    (-> m
                        (update-in [[node1 val1]] conj h)
                        (assoc [node1 val1 node2 val2] h))))
                {} explanatory)
        hyps-explains
        (reduce (fn [m h]
                  (let [{:keys [node1 val1 node2 val2 delta]} (:data h)
                        observed (get observed-hyps [node2 val2])
                        explains (or (get m [node2 val2]) [])
                        obs-expl (if observed (conj explains observed)
                                     explains)
                        h2 (assoc h :explains obs-expl
                                  :data {:node node1 :value val1})]
                    (if (empty? obs-expl) m
                      (-> m
                          (update-in [[node1 val1]] conj h2)
                          (assoc [node1 val1 node2 val2] h2)))))
                {} (get-hyps hyps-no-explains))]
    (filter identity (get-hyps hyps-explains))))

(defn hypothesize
  [ep-state sensors time-now]
  (binding [compute 0 memory 0]
    (let [{:keys [network believed]} (:problem-data ep-state)
          sensor-observed (filter not-empty (mapcat (fn [s] (map (fn [t] (sensed-at s t))
                                                                 (range 0 (inc time-now))))
                                                    sensors))
          bn (build-bayesnet network)
          observed (concat sensor-observed (for [n (keys believed)]
                                             [n (:value (get believed n))]))
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
          sensor-hyps (reduce (fn [m [n v]]
                                (assoc m [n v]
                                       (new-hyp "Obs" :sensor nil
                                                1.0 :or [] []
                                                (format "%s observed %s" n v)
                                                {:node n :value v})))
                              {} sensor-observed)
          network-trans (transpose network)
          implicated (filter #(not (get believed %))
                             (mapcat #(rest (pre-traverse network-trans %))
                                     (map first observed)))
          explanatory (find-explanatory-assignments bn network implicated observed)
          hyps (make-explanation-hyps explanatory sensor-hyps)]
      (unobserve-all bn)
      (observe-seq bn observed)
      (println (get-explanation bn))
      [(reduce (fn [ep hyp] (add-hyp ep hyp))
               (reduce (fn [ep hyp] (add-fact ep hyp))
                       ep-state (vals sensor-hyps))
               hyps)
       {:compute compute :memory memory}])))

(defn commit-decision
  [pdata accepted rejected unaccepted time-now]
  (let [believed (reduce (fn [b h] (assoc b (:node (:data h))
                                          {:value (:value (:data h)) :hyp h}))
                         (:believed pdata)
                         accepted)]
    (assoc pdata :believed believed)))

(defn retract
  [pdata hyp]
  pdata)

(defn no-explainer-hyps
  [hyps pdata]
  [])
