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
          accepted-type (let [t (:type accepted-hyp)]
                          (cond
                           (= t :meta-bad) :meta-accepted-bad
                           (= t :meta-impossible) :meta-accepted-impossible
                           (= t :meta-impossible-lconf) :meta-accepted-impossible-lconf
                           :else :meta-accepted-none))
          est (:ep-state-tree (:data accepted-hyp))
          meta-hyps (filter #(and (not= :meta-accurate (:type %))
                                  (not= :meta-ep (:type %)))
                            (get-hyps workspace))
          ors-meta (-> or-state
                       (update-in [:resources accepted-type] inc)
                       (assoc-in [:meta-workspaces (:id prev-ep)] workspace))]
      (if (or (= :meta-accurate (:type accepted-hyp))
              ;; don't branch if accepted hyp is not any more confident
              (= (hyp-conf workspace accepted-hyp)
                 (hyp-conf workspace (find-first #(= :meta-accurate (:type %))
                                                 (get-hyps workspace)))))
        (update-explain-cycles ors-meta prev-ep meta-hyps)
        (update-explain-cycles
         (-> ors-meta
             (update-in [:resources :meta-abductions] inc)
             (assoc :ep-state-tree est :ep-state (current-ep-state est)))
         prev-ep meta-hyps)))))
