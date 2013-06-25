(ns retrospect.reason.abduction.gui.abduction-log
  (:import (java.awt GridBagLayout Insets Dimension Font))
  (:import (javax.swing Box JScrollBar JTabbedPane))
  (:import (javax.swing.event HyperlinkEvent HyperlinkEvent$EventType))
  (:import (misc AlphanumComparator))
  (:use [fleet])
  (:use [clj-swing.label])
  (:use [clj-swing.text-field])
  (:use [clj-swing.tree])
  (:use [clj-swing.button])
  (:use [clj-swing.panel])
  (:use [seesaw.core :only [editor-pane listen scrollable scroll!]])
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:require [retrospect.reason.abduction.workspace :as ws])
  (:use [retrospect.epistemicstates :only
         [cur-ep flatten-est]])
  (:use [retrospect.reason.abduction.evaluate :only
         [true-meta-hyp? find-meta-hyps group-hyps-by-true-false classify-error classify-noexp-reason]])
  (:use [retrospect.gui.common])
  (:use [retrospect.state]))

(def abduction-tree-map (ref {}))
(def hyps-true-false (ref nil))
(def meta-hyps-true-false (ref nil))

(def anc (AlphanumComparator.))

(declare load-hyp-info)

(def hyp-info
  (let [e (editor-pane
           :content-type "text/html"
           :editable? false)]
    (listen e :hyperlink
            (fn [e]
              (when (= HyperlinkEvent$EventType/ACTIVATED (.getEventType e))
                (let [[hypid ep-id] (str/split (.getDescription e) #"@")]
                  (load-hyp-info (Integer/parseInt hypid) ep-id)))))
    e))

(defn eps-with-hyp
  [hyp]
  (filter #(get-in % [:workspace :hyp-ids (:id hyp)])
     (flatten-est (:est @or-state))))

(defn hyp-link
  ([hyp ep] (format "<a href=\"%s@%s\">%s</a>" (str (:id hyp)) (str (:id ep)) (str ep)))
  ([hyp] (let [most-recent-ep (last (eps-with-hyp hyp))]
           (format "<a href=\"%s@%s\">%s</a>" (str (:id hyp)) (str (:id most-recent-ep)) (str hyp)))))

(def hyp-info-template
  (fleet [hyp log explains explainers conflicts related noexp? noexp-reason error tf acc rej und eps]
         "<html>
<h1><(str hyp)></h1>
<p><(str/replace (:desc hyp) #\"\n\" \"<br>\")></p>
<p><b><(if tf \"True\" \"False\")></b>, <(cond acc \"Accepted\" rej \"Rejected\" und \"Undecided\")></p>
<(if noexp? \"><p>This is an anomaly (no explainer), reason: <(name noexp-reason)></p><\")>
<p>Error status: <(name error)>.</p>
<h2>Explains:</h2>
<ul>
<(for [e explains] \">
  <li><(hyp-link e)></li>
<\")>
</ul>
<h2>Explainers:</h2>
<(for [es explainers] \">
  <ul>
  <(for [e es] \">
    <li><(hyp-link e)></li>
  <\")>
  </ul>
<\")>
<h2>Conflicts:</h2>
<ul>
<(for [c conflicts] \">
  <li><(hyp-link c)></li>
<\")>
</ul>
<h2>Related hyps:</h2>
<ul>
<(for [r related] \">
  <li><(hyp-link r)></li>
<\")>
</ul>
<h2>Log:</h2>
<p><(str/replace log #\"\n\" \"<br>\")></p>
<h2>Epistemic states with this hyp:</h2>
<ul>
<(for [ep eps] \">
  <li><(hyp-link hyp ep)></li>
<\")>
</html>"))

(defn update-hyp-info
  [workspace hyp meta?]
  (let [hyp-tf? (fn [hyp] (or (and meta? (true-meta-hyp? @truedata hyp))
                             (and (not meta?) ((:oracle-fn @problem) @truedata hyp))))
        explains (sort-by :name anc (ws/explains workspace hyp))
        explainers (for [es (vals (group-by :type (ws/explainers workspace hyp)))]
                     (sort-by :name anc es))
        conflicts (sort-by :name anc (ws/find-conflicts workspace hyp))
        related (sort-by :name anc (map #(ws/lookup-hyp workspace %) (ws/related-hyps workspace hyp)))
        log (ws/hyp-log workspace hyp)
        noexp? ((set (ws/no-explainers workspace)) hyp)
        noexp-reason (classify-noexp-reason workspace hyp)
        meta-hyp? ((:meta-hyp-types @reasoner) (:type hyp))
        error (classify-error workspace (if meta-hyp? @meta-hyps-true-false @hyps-true-false) hyp)]
    (.setText hyp-info (str (hyp-info-template hyp log explains explainers conflicts related
                                               noexp? noexp-reason error
                                               (hyp-tf? hyp)
                                               (ws/accepted? workspace hyp)
                                               (ws/rejected? workspace hyp)
                                               (ws/undecided? workspace hyp)
                                               (eps-with-hyp hyp))))
    (scroll! hyp-info :to :top)))

(defn load-hyp-info
  [hypid ep-id]
  (let [ep (first (filter #(= (:id %) ep-id) (flatten-est (:est @or-state))))
        ws (:workspace ep)
        hyp (ws/lookup-hyp ws hypid)]
    (update-hyp-info ws hyp false)))

(defn list-hyps
  [hyps]
  (apply sorted-map-by anc (mapcat (fn [h] [(:name h) nil]) hyps)))

(defn build-cycle
  [workspace]
  (let [accrej (:accrej workspace)]
    (if (empty? accrej) {}
        {(format "Best %s" (if (:essential? accrej) "essential"
                          (format "delta %.2f" (:delta accrej))))
         {(:name (:best accrej)) nil}
         "Explained" {(:name (:explained accrej)) nil}
         "Alternatives" (list-hyps (:alts accrej))
         "Normalized Aprioris"
         (apply hash-map
                (apply concat
                       (map (fn [i]
                            (let [a (nth (:normalized-aprioris accrej) i)]
                              [(format "%d: %.2f" i a) nil]))
                          (range (count (:normalized-aprioris accrej))))))
         "Accepted" (list-hyps (map #(ws/lookup-hyp workspace %) (:acc accrej)))
         "Rejected" (list-hyps (map #(ws/lookup-hyp workspace %) (:rej accrej)))})))

(defn build-abduction-tree-map
  [est meta?]
  (let [ep-states (flatten-est est)        
        ws-fn (fn [ws]
                (let [tf-fn (fn [hyp] (if meta?
                                       (true-meta-hyp? @truedata hyp)
                                       ((:oracle-fn @problem) @truedata hyp)))]
                  {"Hypotheses"
                   (apply merge
                          (for [t (keys (:hypotheses ws))]
                            (let [all-hyps (get (ws/hypotheses ws) t)
                                  acc-hyps (get (ws/accepted ws) t)
                                  rej-hyps (get (ws/rejected ws) t)
                                  ;; TODO: fix group-by or not-group-by
                                  not-acc-hyps (get (ws/undecided ws) t)
                                  all-tf-hyps (group-by tf-fn all-hyps)
                                  acc-tf-hyps (group-by tf-fn acc-hyps)
                                  rej-tf-hyps (group-by tf-fn rej-hyps)
                                  not-acc-tf-hyps (group-by tf-fn not-acc-hyps)]
                              {(name t)
                               (sorted-map
                                "*" (list-hyps all-hyps)
                                "*/T" (list-hyps (get all-tf-hyps true))
                                "*/F" (list-hyps (get all-tf-hyps false))
                                "Acc" (list-hyps acc-hyps)
                                "Acc/T" (list-hyps (get acc-tf-hyps true))
                                "Acc/F" (list-hyps (get acc-tf-hyps false))
                                "Rej" (list-hyps rej-hyps)
                                "Rej/T" (list-hyps (get rej-tf-hyps true))
                                "Rej/F" (list-hyps (get rej-tf-hyps false))
                                "Und" (list-hyps not-acc-hyps)
                                "Und/T" (list-hyps (get not-acc-tf-hyps true))
                                "Und/F" (list-hyps (get not-acc-tf-hyps false)))})))
                   "Cycle" (build-cycle ws)
                   "No explainers" (list-hyps (ws/no-explainers ws))
                   "Unexplained" (list-hyps (ws/unexplained ws))}))]
    (apply sorted-map-by anc
           (mapcat (fn [ep]
                     (let [tree {"Workspace" (assoc (ws-fn (:workspace ep)) "Log" nil)}]
                       [(str ep)
                        (if-not (:meta-est ep) tree
                                (assoc tree
                                  "Abductive Meta"
                                  (build-abduction-tree-map (:meta-est ep) true)))]))
                   ep-states))))

(defn show-log
  [path]
  (if path
    (let [last-comp (node (. path getLastPathComponent))
          ;; find top-most ep-state
          ep-state (if (> (. path getPathCount) 2)
                     (if-let [ep-id (re-find #"^\d+" (str (. path getPathComponent 1)))]
                       (first (filter #(= (:id %) ep-id) (flatten-est (:est @or-state))))))
          meta-ep-state (if (and (> (. path getPathCount) 3)
                                 (= "Abductive Meta" (str (. path getPathComponent 2))))
                          (if-let [ep-id (re-find #"^\d+"
                                                  (str (. path getPathComponent 3)))]
                            (first (filter #(= (:id %) ep-id)
                                      (flatten-est (:meta-est ep-state))))))
          ep (or meta-ep-state ep-state)
          ws (if ep (:workspace ep))]
      (if (= "Log" last-comp)
        (do
          (.setText hyp-info (str/join "<br>" (reverse (:log ws))))
          (scroll! hyp-info :to :top))
        (let [hyp (if ws (first (filter #(= (:name %) last-comp) (vals (:hyp-ids ws)))))]
          (when hyp (update-hyp-info ws hyp (not (nil? meta-ep-state)))))))))

(defn update-abduction-log
  []
  (let [ws (:workspace (cur-ep (:est @or-state)))
        meta-hyps (find-meta-hyps (:est @or-state))]
    (dosync
     (alter abduction-tree-map (constantly (build-abduction-tree-map (:est @or-state) false)))
     (alter hyps-true-false (constantly (group-hyps-by-true-false (vals (:hyp-ids ws))
                                                                  :type @truedata (:oracle-fn @problem) false)))
     (alter meta-hyps-true-false (constantly (group-hyps-by-true-false meta-hyps :type
                                                                       @truedata true-meta-hyp? true))))))

(defn abduction-log-tab
  []
  (doto
      (split-horizontal
       (doto (tree :name tr
                   :model (mapref-tree-model
                           abduction-tree-map "Epistemic states")
                   :action ([_ _] (show-log (.getSelectionPath tr))))
         (.setFont (Font. "Sans" Font/PLAIN 10)))
       (scrollable hyp-info))
    (.setDividerLocation 250)))
