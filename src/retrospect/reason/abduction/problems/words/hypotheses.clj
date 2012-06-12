(ns retrospect.reason.abduction.problems.words.hypotheses
  (:import (java.util.regex Pattern))
  (:import (org.arabidopsis.ahocorasick AhoCorasick))
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:use [loom.graph :only [has-edge? weight edges neighbors incoming]])
  (:use [retrospect.profile :only [prof]])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.reason.abduction.workspace :only [new-hyp]])
  (:use [retrospect.reason.abduction.problems.words.evaluate :only
         [get-words]])
  (:use [retrospect.logging])
  (:use [retrospect.state])
  (:use [retrospect.random]))

(defn generate-kb
  [training]
  (let [dict-tree (AhoCorasick.)]
    (doseq [w (:test-dict training)]
      (.add dict-tree (.getBytes w) w))
    (.prepare dict-tree)
    [(new-hyp "KB" :kb :kb false (constantly false)
              [] [] "" "" {:dict-tree dict-tree :dict #{}})]))

(defn make-sensor-hyps
  [sensor time-prev time-now accepted lookup-hyp]
  (concat (map (fn [[sym pos]]
               (new-hyp "Symbol" :symbol :symbol true (constantly false)
                        [] []
                        (format "%s" sym)
                        (format "Symbol: %s, pos: %d" sym pos)
                        {:pos pos :sym sym}))
             (sensed-at sensor (inc time-prev)))
          (map (fn [[[sym1 pos1] [sym2 pos2]]]
               (new-hyp "Transition" :transition :transition true (constantly false)
                        [] []
                        (format "%s/%s" (str sym1) (str sym2))
                        (format "Transition: %s/%s, pos: %d/%d"
                           (str sym1) (str sym2) pos1 pos2)
                        {:sym1 sym1 :sym2 sym2
                         :pos1 pos1 :pos2 pos2}))
             (partition 2 1 (sensed-at sensor (inc time-prev))))))

(defn get-hyp-types
  []
  (set (str/split (:HypTypes params) #",")))

(defn conflicts?
  [hyp1 hyp2]
  (cond
   (= (:id hyp1) (:id hyp2)) false
   
   (or (= :symbol (:type hyp1)) (= :symbol (:type hyp2))) false
   (or (= :transition (:type hyp1)) (= :transition (:type hyp2))) false

   (or (and (= :word (:type hyp1)) (= :notword (:type hyp2)))
       (and (= :word (:type hyp2)) (= :notword (:type hyp1))))
   (= (:explains hyp1) (:explains hyp2))

   (and (or (= :merge (:type hyp1)) (= :merge (:type hyp2)))
        (or (= :split (:type hyp1)) (= :split (:type hyp2))))
   (= (:pos1 hyp1) (:pos1 hyp2))

   (and (or (= :word (:type hyp1)) (= :word (:type hyp2)))
        (or (= :merge (:type hyp1)) (= :merge (:type hyp2))))
   (let [word-hyp (if (= :word (:type hyp1)) hyp1 hyp2)
         merge-hyp (if (= :merge (:type hyp1)) hyp1 hyp2)
         word-right? (= :right (second (:subtype word-hyp)))
         word-left? (= :left (first (:subtype word-hyp)))]
     (or (and (not word-left?) (not word-right?)
              (or (= (:pos2 merge-hyp) (first (:pos-seq word-hyp)))
                  (= (:pos1 merge-hyp) (last (:pos-seq word-hyp)))))
         (and word-left? (not word-right?)
              (= (:pos1 merge-hyp) (last (:pos-seq word-hyp))))
         (and (not word-left?) word-right?
              (= (:pos2 merge-hyp) (first (:pos-seq word-hyp))))))

   (and (or (= :word (:type hyp1)) (= :word (:type hyp2)))
        (or (= :split (:type hyp1)) (= :split (:type hyp2))))
   (let [word-hyp (if (= :word (:type hyp1)) hyp1 hyp2)
         split-hyp (if (= :split (:type hyp1)) hyp1 hyp2)
         word-right? (= :right (second (:subtype word-hyp)))
         word-left? (= :left (first (:subtype word-hyp)))]
     (or (and (< (:pos1 split-hyp) (last (:pos-seq word-hyp)))
              (> (:pos2 split-hyp) (first (:pos-seq word-hyp))))
         (and word-left? (not word-right?)
              (= (:pos2 split-hyp) (first (:pos-seq word-hyp))))
         (and (not word-left?) word-right?
              (= (:pos1 split-hyp) (last (:pos-seq word-hyp))))))
   
   (and (= :word (:type hyp1)) (= :word (:type hyp2)))
   (let [first-word (if (<= (first (:pos-seq hyp1)) (first (:pos-seq hyp2)))
                      hyp1 hyp2)
         second-word (if (<= (first (:pos-seq hyp1)) (first (:pos-seq hyp2)))
                       hyp2 hyp1)
         start1 (first (:pos-seq first-word))
         end1 (last (:pos-seq first-word))
         start2 (first (:pos-seq second-word))
         end2 (last (:pos-seq second-word))
         first-right? (= :right (second (:subtype first-word)))
         second-left? (= :left (first (:subtype second-word)))]
     (cond
      ;; right,left any overlap is a conflict
      (and first-right? second-left?)
      (and (>= end1 start2) (>= end2 start1))
      ;; right,(not left): any overlap plus following pos of next word
      (and first-right? (not second-left?))
      (and (>= end1 (dec start2)) (>= end2 start1))
      ;; (not right),left: any overlap plus previous pos of prior word
      (and (not first-right?) second-left?)
      (and (>= end1 start2) (>= end2 (dec start1)))
      ;; (not right),(not left): any overlap
      :else
      (and (>= end1 start2) (>= end2 start1))))

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
              (if training? (filter #(dict (first %)) found) found)
              (let [result (.next searcher)
                    last-index (.getLastIndex result)]
                (recur (reduce (fn [fs w]
                            (conj fs [w (int (- (/ last-index 3) (count w)))]))
                          found (.getOutputs result)))))))))

(defn hypothesize
  [forced-hyps accepted lookup-hyp]
  (let [kb (get-kb accepted lookup-hyp)
        hyp-types (get-hyp-types)
        transition-hyps (sort-by :pos1 (filter #(= :transition (:type %)) forced-hyps))
        merge-split-hyps (mapcat
                          (fn [t-hyp]
                            (map (fn [type]
                                 (new-hyp (if (= :merge type) "Merge" "Split")
                                          type [(:sym1 t-hyp) (:sym2 t-hyp)]
                                          false conflicts?
                                          [t-hyp] [] ;; no boosting
                                          (format "%s%s%s"
                                             (str (:sym1 t-hyp))
                                             (if (= :merge type) "+" "-")
                                             (str (:sym2 t-hyp)))
                                          (format "%s%s%s at %d/%d"
                                             (str (:sym1 t-hyp))
                                             (if (= :merge type) "+" "-")
                                             (str (:sym2 t-hyp))
                                             (:pos1 t-hyp) (:pos2 t-hyp))
                                          {:pos1 (:pos1 t-hyp) :pos2 (:pos2 t-hyp)
                                           :sym1 (:sym1 t-hyp) :sym2 (:sym2 t-hyp)}))
                               [:merge :split]))
                          transition-hyps)
        symbol-hyps (vec (sort-by :pos (filter #(= :symbol (:type %)) forced-hyps)))
        sym-string (apply str (map :sym symbol-hyps))
        words (doall
               (filter not-empty
                  (map (fn [[w i]]
                       (subvec symbol-hyps (max 0 i) (min (count symbol-hyps)
                                                          (+ i (count w)))))
                     (find-dict-words sym-string (:dict-tree kb) (:dict kb)))))
        word-hyps
        (prof :word-hyps
              (doall
               (mapcat
                (fn [s-hyps]
                  (let [word (apply str (map :sym s-hyps))
                        pos-seq (map :pos s-hyps)]
                    (mapcat (fn [subtype]
                            (map (fn [type]
                                 (new-hyp
                                  (format "%sWord%s"
                                     (if (= :notword type) "Not" "")
                                     (cond (= [:left :right] (take 2 subtype)) "LR"
                                           (= :left (first subtype)) "L"
                                           (= :right (second subtype)) "R"
                                           :else ""))
                                  type subtype false conflicts?
                                  s-hyps [] ;; no boosting
                                  (format "%s%s%s%s"
                                     (if (= :notword type) "!" "")
                                     (if (= :left (first subtype)) "#" "")
                                     word
                                     (if (= :right (second subtype)) "#" ""))
                                  (format "Word: %s, pos-seq: %s\nsubtype: %s"
                                     word (str/join ", " (map str pos-seq))
                                     subtype)
                                  {:pos-seq pos-seq :word word}))
                               [:word :notword]))
                          (if (hyp-types "wordslr")
                            [[word]
                             [:left word]
                             [word :right]
                             [:left :right word]]
                            [[word]]))))
                (sort-by (comp :pos first) words))))]
    (concat merge-split-hyps word-hyps)))
