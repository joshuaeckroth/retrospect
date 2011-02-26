(ns samre.meta.explain
  (:require [samre logs])
  (:import [samre.logs MetaLogEntry])
  (:use [samre.workspaces :only [init-workspace explain lookup-hyps]])
  (:use [samre.epistemicstates :only
         [current-ep-state previous-ep-state update-ep-state-tree]])
  (:use [samre.onerun :only [update-one-run-state]])
  (:use [samre.meta.hypotheses :only [generate-meta-hypotheses]]))

(defn update-meta-log
  [or-state ep-state workspace]
  (update-in or-state [:meta-log] conj
             (MetaLogEntry. (:ep-state or-state) ep-state
                            (:abducer-log workspace))))

(defn update-explain-cycles
  [or-state ep-state meta-hyps]
  (update-one-run-state
   or-state
   (update-in ep-state [:workspace :resources] assoc :explain-cycles
              (reduce + (:explain-cycles (:resources (:workspace (:ep-state or-state))))
                      (map (comp :explain-cycles :data) meta-hyps)))))

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
            est (:ep-state-tree (:data accepted-hyp))
            meta-hyps (filter #(and (not= :meta-accurate (:type %))
                                    (not= :meta-ep (:type %)))
                              (vals (:hyps workspace)))
            ors (update-explain-cycles or-state (:ep-state or-state) meta-hyps)
            ors-log (update-meta-log ors (:ep-state ors) workspace)]
        (if (= :meta-accurate (:type accepted-hyp)) ors-log
          (assoc (update-in ors-log [:resources :meta-abductions] inc)
            :ep-state-tree est :ep-state (current-ep-state est))))))
