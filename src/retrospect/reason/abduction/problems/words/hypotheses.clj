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
  (:use [retrospect.state]))

(defn generate-kb
  [training]
  (let [dict-tree (AhoCorasick.)]
    (doseq [w (:test-dict training)]
      (.add dict-tree (.getBytes w) w))
    (.prepare dict-tree)
    [(new-hyp "KB" :kb :kb false [] [] "" "" {:dict-tree dict-tree})]))

(defn make-sensor-hyps
  [sensor time-prev time-now hyps]
  (map (fn [[sym pos]]
         (new-hyp "Symbol" :symbol :symbol true [] []
                  (format "%s" sym)
                  (format "Symbol: %s, pos: %d" sym pos)
                  {:pos pos :sym sym}))
       (sensed-at sensor (inc time-prev))))

(defn get-hyp-types
  []
  (set (str/split (:HypTypes params) #",")))

(defn conflicts?
  [hyp1 hyp2]
  (cond
   (= (:id hyp1) (:id hyp2)) false
   
   (or (= :symbol (:type hyp1)) (= :symbol (:type hyp2))) false
   
   (and (= :word (:type hyp1)) (= :word (:type hyp2)))
   (let [[first-word second-word] (sort-by (comp first :pos-seq) [hyp1 hyp2])
         start1 (first (:pos-seq first-word))
         end1 (last (:pos-seq first-word))
         start2 (first (:pos-seq second-word))
         end2 (last (:pos-seq second-word))]
     (cond
      ;; right,left any overlap is a conflict
      (and (= :right (second (:subtype first-word)))
           (= :left (first (:subtype second-word))))
      (and (>= end1 start2) (>= end2 start1))
      ;; right,(not left): any overlap plus following pos of next word
      (and (= :right (second (:subtype first-word)))
           (not= :left (first (:subtype second-word))))
      (and (>= end1 (dec start2)) (>= end2 start1))
      ;; (not right),left: any overlap plus previous pos of prior word
      (and (not= :right (second (:subtype first-word)))
           (= :left (first (:subtype second-word))))
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
  (map lookup-hyp (get accepted :kb)))

(defn find-dict-words
  [sym-string dict-tree]
  (prof :find-dict-words
        (let [sym-count (count sym-string)
              sym-bytes (.getBytes sym-string)
              searcher (.search dict-tree sym-bytes)]
          (loop [found []]
            (if-not (.hasNext searcher) found
                    (let [result (.next searcher)
                          last-index (.getLastIndex result)]
                      (recur (reduce (fn [fs w]
                                       (conj fs [w (int (- (/ last-index 3) (count w)))]))
                                     found (.getOutputs result)))))))))

(defn hypothesize
  [forced-hyps accepted lookup-hyp]
  (let [kb (get-kb accepted lookup-hyp)
        hyp-types (get-hyp-types)
        symbol-hyps (vec (sort-by :pos forced-hyps))
        sym-string (apply str (map :sym symbol-hyps))
        words (filter not-empty
                      (map (fn [[w i]]
                             (subvec symbol-hyps (max 0 i) (min (count symbol-hyps)
                                                                (+ i (count w)))))
                           (find-dict-words sym-string (:dict-tree kb))))
        word-hyps
        (prof :word-hyps
              (filter
               #(not-empty (:explains %))
               (mapcat
                (fn [s-hyps]
                  (let [word (apply str (map :sym s-hyps))
                        pos-seq (map :pos s-hyps)]
                    (map (fn [subtype]
                           (new-hyp
                            (format "Word%s" (cond (= [:left :right] (take 2 subtype)) "LR"
                                                   (= :left (first subtype)) "L"
                                                   (= :right (second subtype)) "R"
                                                   :else ""))
                            :word subtype false
                            s-hyps [] ;; no boosting
                            (format "%s%s%s"
                                    (if (= :left (first subtype)) "#" "")
                                    word
                                    (if (= :right (second subtype)) "#" ""))
                            (format "Word: %s, pos-seq: %s\nsubtype: %s"
                                    word (str/join ", " (map str pos-seq))
                                    subtype)
                            {:pos-seq pos-seq :word word}))
                         (if (hyp-types "wordslr")
                           [[word]
                            [:left word]
                            [word :right]
                            [:left :right word]]
                           [[word]]))))
                (sort-by (comp :pos first) words))))]
    (doall (map (fn [h] (assoc h :conflicts
                               (doall (map :id (filter #(conflicts? h %) word-hyps)))))
                word-hyps))))
