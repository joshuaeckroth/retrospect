(ns retrospect.reason.abduction.problems.words.hypotheses
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:use [loom.graph :only [has-edge? weight edges neighbors incoming]])
  (:use [retrospect.profile :only [prof]])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.reason.abduction.workspace :only [new-hyp]])
  (:use [retrospect.reason.abduction.problems.words.evaluate :only
         [get-words]])
  (:use [retrospect.problems.words.truedata :only [extract-tags-word]])
  (:use [retrospect.logging])
  (:use [retrospect.state])
  (:use [retrospect.random]))

(defn generate-kb
  [training]
  [(new-hyp "KB" :kb :kb 1.0 false nil
            [] "" "" {:dict-tree (:dict-tree training) :dict (:dict training)
                      :scores (:scores training)})])

(defn make-sensor-hyps
  [sensor time-prev time-now accepted lookup-hyp]
  (map (fn [[sym pos]]
       (new-hyp "Symbol" :symbol :symbol 1.0 true nil
                [] (format "%s" sym) (format "Symbol: %s, pos: %d" sym pos)
                {:pos pos :sym sym :time (inc time-prev)}))
     (sensed-at sensor (inc time-prev))))

(defn conflicts?
  [hyp1 hyp2]
  (cond
   (= (:id hyp1) (:id hyp2)) false
   (or (= :symbol (:type hyp1)) (= :symbol (:type hyp2))) false
   (and (= :tag (:type hyp1)) (= :tag (:type hyp2)))
   (or (= (:pos hyp1) (:pos hyp2))
       (let [[first-hyp second-hyp] (if (< (:pos hyp1) (:pos hyp2))
                                        [hyp1 hyp2] [hyp2 hyp1])]
         (and (= (inc (:pos first-hyp)) (:pos second-hyp))
              (or (and (= :Start (:tag first-hyp))
                       (not (#{:Middle :End} (:tag second-hyp))))
                  (and (= :Middle (:tag first-hyp))
                       (not (#{:Middle :End} (:tag second-hyp))))
                  (and (= :End (:tag first-hyp))
                       (not (#{:Start :Only} (:tag second-hyp))))
                  (and (= :Only (:tag first-hyp))
                       (not (#{:Start :Only} (:tag second-hyp))))))))
   (and (= :word (:type hyp1)) (= :word (:type hyp2)))
   (let [[first-word second-word] (if (<= (first (:pos-seq hyp1)) (first (:pos-seq hyp2)))
                                    [hyp1 hyp2] [hyp2 hyp1])
         start1 (first (:pos-seq first-word))
         end1 (last (:pos-seq first-word))
         start2 (first (:pos-seq second-word))
         end2 (last (:pos-seq second-word))]
     (and (>= end1 start2) (>= end2 start1)))
   (or (and (= :word (:type hyp1)) (= :tag (:type hyp2)))
       (and (= :word (:type hyp2)) (= :tag (:type hyp1))))
   (let [[word-hyp tag-hyp] (if (= :word (:type hyp1))
                              [hyp1 hyp2] [hyp2 hyp1])
         word-tags (zipmap (:pos-seq word-hyp)
                           (map second (extract-tags-word (:word word-hyp))))
         tag (:tag tag-hyp)
         tag-pos (:pos tag-hyp)]
     (and (<= tag-pos (last (:pos-seq word-hyp)))
          (>= tag-pos (first (:pos-seq word-hyp)))
          (not= (get word-tags tag-pos) tag)))
   :else false))

(defn get-kb
  [accepted lookup-hyp]
  (lookup-hyp (first (get accepted :kb))))

(defn update-kb
  [accepted unexplained hypotheses lookup-hyp]
  (let [symbol-hyps (map lookup-hyp (get hypotheses :symbol))
        sym-string (apply str (map :sym (sort-by :pos symbol-hyps)))
        words (get-words lookup-hyp sym-string accepted unexplained)
        old-kb (get-kb accepted lookup-hyp)
        new-dict (reduce conj (:dict old-kb) words)]
    [(assoc old-kb :dict new-dict)]))

(defn find-dict-words
  [sym-string dict-tree dict]
  (prof :find-dict-words
        (let [sym-count (count sym-string)
              sym-bytes (.getBytes sym-string)
              searcher (.search dict-tree sym-bytes)]
          (loop [found []]
            (if-not (.hasNext searcher)
              (filter #(dict (first %)) found)
              (let [result (.next searcher)
                    last-index (.getLastIndex result)]
                (recur (reduce (fn [fs w]
                            (conj fs [w (int (- (/ last-index 3) (count w)))]))
                          found (.getOutputs result)))))))))

(defn hypothesize
  [forced-hyps accepted lookup-hyp]
  (let [kb (get-kb accepted lookup-hyp)
        symbol-hyps (vec (sort-by :pos forced-hyps))
        sym-string (apply str (map :sym symbol-hyps))
        time-now (:time (first symbol-hyps))
        scores (nth (:scores kb) (dec time-now))
        words (doall (filter not-empty
                        (map (fn [[w i]]
                             (subvec symbol-hyps (max 0 i) (min (count symbol-hyps)
                                                                (+ i (count w)))))
                           (find-dict-words sym-string (:dict-tree kb) (:dict kb)))))]
    (concat
     ;; tag hyps
     (mapcat
      (fn [s-hyp]
        (let [[_ tag-apriori] (nth scores (:pos s-hyp))]
          (map (fn [[tag apriori]]
               (new-hyp tag :tag tag apriori
                        false conflicts? [s-hyp]
                        (format "%s:%s" tag (:sym s-hyp))
                        (format "%s:%s" tag (:sym s-hyp))
                        {:pos (:pos s-hyp) :sym (:sym s-hyp) :tag tag}))
             tag-apriori)))
      symbol-hyps)
     ;; word hyps
     (map (fn [s-hyps]
          (let [word (apply str (map :sym s-hyps))
                pos-seq (map :pos s-hyps)]
            (new-hyp "Word" :word :word 0.5
                     false conflicts? s-hyps
                     word (format "%s\npos-seq: %s" word (str/join "," pos-seq))
                     {:word word :pos-seq pos-seq})))
        (sort-by (comp :pos first) words)))))
