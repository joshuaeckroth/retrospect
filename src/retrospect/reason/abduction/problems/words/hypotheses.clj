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
  []
  [(new-hyp "KB" :kb :kb false [] [] "" ""
            {:dict-tree (doto (AhoCorasick.) (.prepare))
             :dict #{}})])

(defn make-sensor-hyps
  [sensor time-prev time-now hyps]
  (map (fn [[[sym1 pos1] [sym2 pos2]]]
         (new-hyp "Trans" :transition :transition true [] []
                  (format "%s/%s" sym1 sym2)
                  (format "Transition: %s/%s, pos-seq: %d-%d"
                          sym1 sym2 pos1 pos2)
                  {:pos-seq [pos1 pos2]
                   :trans-pos pos1
                   :sym1 sym1 :sym2 sym2}))
       (partition 2 1 (sensed-at sensor (inc time-prev)))))

(defn conflicts?
  [hyp1 hyp2]
  (cond
   (= hyp1 hyp2) false
   
   (or (= :transition (:type hyp1)) (= :transition (:type hyp2))) false
   
   (and (= :word (:type hyp1)) (= :word (:type hyp2)))
   (let [start1 (first (:pos-seq hyp1))
         end1 (last (:pos-seq hyp1))
         start2 (first (:pos-seq hyp2))
         end2 (last (:pos-seq hyp2))]
     (not (or (< end1 start2) (< end2 start1))))
   
   (and (= :word (:type hyp1)) (= :split (:type hyp2)))
   (and (>= (:trans-pos hyp2) (first (:pos-seq hyp1)))
        (<= (:trans-pos hyp2) (dec (last (:pos-seq hyp1)))))

   (and (= :word (:type hyp2)) (= :split (:type hyp1)))
   (and (>= (:trans-pos hyp1) (first (:pos-seq hyp2)))
        (<= (:trans-pos hyp1) (dec (last (:pos-seq hyp2)))))
   
   (and (= :word (:type hyp2)) (= :merge (:type hyp1)))
   (or (= (:trans-pos hyp1) (dec (first (:pos-seq hyp2))))
       (= (:trans-pos hyp1) (last (:pos-seq hyp2))))

   (and (= :word (:type hyp1)) (= :merge (:type hyp2)))
   (or (= (:trans-pos hyp2) (dec (first (:pos-seq hyp1))))
       (= (:trans-pos hyp2) (last (:pos-seq hyp1))))

   (or (and (= :split (:type hyp1)) (= :merge (:type hyp2)))
       (and (= :merge (:type hyp1)) (= :split (:type hyp2))))
   (= (:trans-pos hyp1) (:trans-pos hyp2))
   
   :else false))

(defn get-hyp-types
  []
  (set (str/split (:HypTypes params) #",")))

(defn get-kb
  [accepted lookup-hyp]
  (lookup-hyp (first (get accepted :kb))))

(defn update-dict-tree
  [dict-tree words]
  (.reset dict-tree)
  (doseq [w words]
    (.add dict-tree (.getBytes w) w))
  (.prepare dict-tree))

(defn update-kb
  [accepted unexplained hypotheses lookup-hyp]
  (if (not ((get-hyp-types) "words")) (map lookup-hyp (get accepted :kb))
      (let [kb (get-kb accepted lookup-hyp)
            dict (:dict kb)
            transition-hyps (sort-by :trans-pos (map lookup-hyp (get accepted :transition)))
            words (get-words lookup-hyp (apply str (map :sym1 transition-hyps))
                             accepted unexplained)
            new-dict (set/union dict (set words))]
        (update-dict-tree (:dict-tree kb) new-dict)
        [(assoc kb :dict new-dict
                :contents (assoc (:contents kb) :dict new-dict))])))

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
                                       (conj fs [w (- (/ last-index 3) (count w))]))
                                     found (.getOutputs result)))))))))

(defn hypothesize
  [forced-hyps accepted lookup-hyp]
  (let [kb (get-kb accepted lookup-hyp)
        hyp-types (get-hyp-types)
        transition-hyps (vec (sort-by :trans-pos forced-hyps))
        merge-hyps
        (prof :merge-hyps
              (map
               (fn [t-hyp]
                 (new-hyp "Merge" :merge {:sym1 (:sym1 t-hyp) :sym2 (:sym2 t-hyp)} false
                          [t-hyp] [] (format "%s+%s" (:sym1 t-hyp) (:sym2 t-hyp))
                          (format (str "Merge of %s+%s at %d")
                                  (:sym1 t-hyp)
                                  (:sym2 t-hyp)
                                  (:trans-pos t-hyp))
                          {:trans-pos (:trans-pos t-hyp)}))
               transition-hyps))
        split-hyps
        (prof :split-hyps
              (map
               (fn [m-hyp]
                 (let [t-hyp (first (:explains m-hyp))]
                   (new-hyp "Split" :split {:sym1 (:sym1 t-hyp) :sym2 (:sym2 t-hyp)} false
                            [t-hyp] [] (format "%s-%s" (:sym1 t-hyp)
                                               (:sym2 t-hyp))
                            (format (str "Split of %s-%s at %d")
                                    (:sym1 t-hyp)
                                    (:sym2 t-hyp)
                                    (:trans-pos t-hyp))
                            {:trans-pos (:trans-pos t-hyp)})))
               merge-hyps))
        sym-string (apply str (map :sym1 transition-hyps))
        words (map (fn [[w i]] (subvec transition-hyps i (+ i (count w))))
                   (find-dict-words sym-string (:dict-tree kb)))
        word-hyps
        (prof :word-hyps
              (filter
               #(not-empty (:explains %))
               (map (fn [t-hyps]
                      (let [word (apply str (map :sym1 t-hyps))
                            pos-seq (map :trans-pos t-hyps)
                            similar-words (map #(apply str (map :sym1 %))
                                               (filter
                                                #(and (>= (:trans-pos (first t-hyps))
                                                          (:trans-pos (first %)))
                                                      (<= (:trans-pos (last t-hyps))
                                                          (:trans-pos (last %))))
                                                words))]
                        (new-hyp "Word" :word word false t-hyps [] ;; no boosting
                                 word (format "Word: %s, pos-seq: %s\nsimilar: %s" word
                                              (str/join ", " (map str pos-seq))
                                              (str/join ", " similar-words))
                                 {:pos-seq pos-seq :word word})))
                    (sort-by (comp :trans-pos first) words))))
        hyps (concat (if (hyp-types "mergesplit") (concat merge-hyps split-hyps) [])
                     (if (hyp-types "words") word-hyps []))]
    (doall (map (fn [h] (assoc h :conflicts
                               (map :id (filter #(conflicts? h %) hyps))))
                hyps))))
