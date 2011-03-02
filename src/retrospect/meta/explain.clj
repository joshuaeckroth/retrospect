(ns retrospect.meta.explain
  (:use [retrospect.workspaces :only [init-workspace prepare-workspace explain get-hyps]])
  (:use [retrospect.epistemicstates :only
         [current-ep-state previous-ep-state update-ep-state-tree]])
  (:use [retrospect.onerun :only [update-one-run-state]])
  (:use [retrospect.meta.hypotheses :only [generate-meta-hypotheses]]))

(defn update-explain-cycles
  [or-state ep-state meta-hyps]
  (let [expl-cyc-prior (:explain-cycles (:resources (:workspace (:ep-state or-state))))
        meta-expl-cycs (map (comp :explain-cycles :data) meta-hyps)]
    (update-one-run-state or-state
                          (update-in ep-state [:workspace :resources] assoc :explain-cycles
                                     (reduce + expl-cyc-prior meta-expl-cycs)))))

(defn explain-meta
  [problem or-state params]
  (if (not (:meta-abduction or-state)) or-state
      (let [workspace (-> (init-workspace)
                          (generate-meta-hypotheses problem (:ep-state-tree or-state)
                                                    (:sensors or-state) params
                                                    (:lazy or-state))
                          (prepare-workspace)
                          (explain))
            ;; we only expect one accepted meta hyp
            accepted-hyp (first (:accepted workspace))
            est (:ep-state-tree (:data accepted-hyp))
            meta-hyps (filter #(and (not= :meta-accurate (:type %))
                                    (not= :meta-ep (:type %)))
                              (get-hyps workspace))
            ors (update-explain-cycles or-state (:ep-state or-state) meta-hyps)]
        (if (= :meta-accurate (:type accepted-hyp)) ors
          (assoc (update-in ors [:resources :meta-abductions] inc)
            :ep-state-tree est :ep-state (current-ep-state est))))))
