(ns retrospect.meta.explain
  (:use [retrospect.workspaces :only
         [meta? init-workspace prepare-workspace explain get-hyps hyp-conf get-conf]])
  (:use [retrospect.epistemicstates :only
         [current-ep-state previous-ep-state update-ep-state-tree]])
  (:use [retrospect.onerun :only [update-one-run-state]])
  (:use [retrospect.meta.hypotheses :only [generate-meta-hypotheses]])
  (:use [clojure.contrib.seq :only [find-first]]))

(defn update-explain-cycles
  [or-state prev-ep meta-hyps]
  (let [expl-cyc-prior (:explain-cycles (:resources (:workspace prev-ep)))
        meta-expl-cycs (map (comp :explain-cycles :data) meta-hyps)]
    (update-one-run-state
     or-state (update-in (:ep-state or-state)
                         [:workspace :resources] assoc :explain-cycles
                         (reduce + expl-cyc-prior meta-expl-cycs)))))

(def meta-types
     [:MetaBad :MetaImpossible :MetaImpossibleLconf :MetaBatch :MetaNone])

(defn explain-meta
  [problem or-state bad params]
  (binding [meta? true]
    (let [prev-ep (previous-ep-state (:ep-state-tree or-state))
          workspace (-> (init-workspace)
                        (generate-meta-hypotheses problem (:ep-state-tree or-state)
                                                  (:sensors or-state) bad params
                                                  (:lazy or-state))
                        (prepare-workspace)
                        (explain (constantly []) nil))
          ;; we only expect one accepted meta hyp
          accepted-hyp (first (:accepted workspace))
          est (:ep-state-tree (:data accepted-hyp))
          meta-hyps (filter #(and (not= :MetaNone (:type %))
                                  (not= :MetaEP (:type %)))
                            (get-hyps workspace))
          ors-meta (assoc-in or-state [:meta-workspaces (:id prev-ep)] workspace)]
      (if (or (= :MetaNone (:type accepted-hyp))
              ;; don't branch if accepted hyp is not somewhat more confident
              (<= 0.2 (- (hyp-conf workspace accepted-hyp) 
                         (hyp-conf workspace (find-first #(= :MetaNone (:type %))
                                                         (get-hyps workspace))))))
        [(update-explain-cycles ors-meta prev-ep meta-hyps) :MetaNone 0]
        [(update-explain-cycles
          (-> ors-meta
              (update-in [:resources :meta-abductions] inc)
              (update-in [:resources (:type accepted-hyp)] inc)
              (assoc :ep-state-tree est :ep-state (current-ep-state est)))
          prev-ep meta-hyps)
         (:type accepted-hyp) (:diff-time (:data accepted-hyp))]))))

