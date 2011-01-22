(ns samre.meta.explain
  (:use [samre.workspaces :only [init-workspace explain lookup-hyps]])
  (:use [samre.epistemicstates :only [current-ep-state]])
  (:use [samre.meta.hypotheses :only [generate-meta-hypotheses]]))

(defn explain-meta
  [problem or-state params]
  (if (not (:meta-abduction or-state)) or-state
      (let [workspace (-> (init-workspace)
                          (generate-meta-hypotheses problem (:ep-state-tree or-state)
                                                    (:sensors or-state) params
                                                    (:lazy or-state))
                          (explain))
            ors (update-in or-state [:meta-log] conj (:abducer-log workspace))
            accepted-hyps (lookup-hyps workspace (:accepted (:decision workspace)))
            new-est (reduce (fn [est action] (action est))
                            (:ep-state-tree ors)
                            (map :update-fn accepted-hyps))
            ors-resources (if (and (= 1 (count accepted-hyps))
                                   (= :MH-dec-accurate (:id (first accepted-hyps))))
                            ors
                            (update-in or-state [:resources :meta-abductions] inc))]
        (assoc ors-resources :ep-state-tree new-est :ep-state (current-ep-state new-est)))))
