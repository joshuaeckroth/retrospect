(ns samre.meta.explain
  (:require [samre logs])
  (:import [samre.logs MetaLogEntry])
  (:use [samre.workspaces :only [init-workspace explain lookup-hyps]])
  (:use [samre.epistemicstates :only
         [current-ep-state previous-ep-state update-ep-state-tree]])
  (:use [samre.meta.hypotheses :only [generate-meta-hypotheses]]))

(defn update-meta-log
  [or-state ep-state workspace]
  (update-in or-state [:meta-log] conj
             (MetaLogEntry. (:ep-state or-state) ep-state
                            (:abducer-log workspace))))

(defn explain-meta
  [problem or-state params]
  (if (not (:meta-abduction or-state)) or-state
      (let [workspace (-> (init-workspace)
                          (generate-meta-hypotheses problem (:ep-state-tree or-state)
                                                    (:sensors or-state) params
                                                    (:lazy or-state))
                          (explain))
            ;; we only expect one accepted meta hyp
            accepted-hyp (first (lookup-hyps workspace (:accepted (:decision workspace))))
            est (:ep-state-tree (:data accepted-hyp))]
        (if (= :meta-accurate (:type accepted-hyp))
          (update-meta-log or-state (:ep-state or-state) workspace)
          (let [ep-state (current-ep-state est)
                ors (update-meta-log or-state ep-state workspace)]
            (assoc (update-in ors [:resources :meta-abductions] inc)
              :ep-state-tree est :ep-state ep-state))))))
