(ns samre.meta.explain
  (:use [samre.workspaces :only [init-workspace explain lookup-hyps]])
  (:use [samre.epistemicstates :only [current-ep-state update-ep-state-tree]])
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
            ;; we only expect one accepted meta hyp
            accepted-hyp (first (lookup-hyps workspace (:accepted (:decision workspace))))
            new-est (update-ep-state-tree (:ep-state-tree (:data accepted-hyp))
                                          (:ep-state (:data accepted-hyp)))]
        (if (= :meta-accurate (:type accepted-hyp)) ors
            (assoc (update-in ors [:resources :meta-abductions] inc)
              :ep-state-tree new-est :ep-state (current-ep-state new-est))))))
