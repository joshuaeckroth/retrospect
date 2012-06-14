(ns retrospect.reason.abduction.problems.words.hypotheses
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
  [(new-hyp "KB" :kb :kb {} 1.0 false nil
            [] [] "" "" {:dict-tree (:dict-tree training) :dict #{}})])

(defn make-sensor-hyps
  [sensor time-prev time-now accepted lookup-hyp]
  (map (fn [[sym pos]]
       (new-hyp "Symbol" :symbol :symbol {} 1.0 true nil
                [] []
                (format "%s" sym)
                (format "Symbol: %s, pos: %d" sym pos)
                {:pos pos :sym sym}))
     (sensed-at sensor (inc time-prev))))

(defn conflicts?
  [hyp1 hyp2]
  (cond
   (= (:id hyp1) (:id hyp2)) false
   (or (= :symbol (:type hyp1)) (= :symbol (:type hyp2))) false
   (and (= :tag (:type hyp1)) (= :tag (:type hyp2)))
   (= (:pos hyp1) (:pos hyp2))
   :else false))

(defn get-kb
  [accepted lookup-hyp]
  (lookup-hyp (first (get accepted :kb))))

(defn update-kb
  [accepted unexplained hypotheses lookup-hyp]
  (map lookup-hyp (get accepted :kb)))

(defn sym-at-offset
  [offset symbol-hyps pos]
  (:sym (first (filter #(= (+ offset pos) (:pos %)) symbol-hyps))))

(defn hypothesize
  [forced-hyps accepted lookup-hyp]
  (let [kb (get-kb accepted lookup-hyp)
        symbol-hyps (sort-by :pos forced-hyps)]
    (mapcat
     (fn [s-hyp]
       (let [features {:left2 (sym-at-offset -2 symbol-hyps (:pos s-hyp))
                       :left1 (sym-at-offset -1 symbol-hyps (:pos s-hyp))
                       :self (sym-at-offset 0 symbol-hyps (:pos s-hyp))
                       :right1 (sym-at-offset 1 symbol-hyps (:pos s-hyp))
                       :right2 (sym-at-offset 2 symbol-hyps (:pos s-hyp))}]
         (map (fn [tag]
              (new-hyp (name tag) :tag tag features 0.5
                       false conflicts? [s-hyp] []
                       (format "%s:%s" (name tag) (:sym s-hyp))
                       (format "%s:%s\nFeatures: %s" (name tag) (:sym s-hyp) (str features))
                       {:pos (:pos s-hyp) :sym (:sym s-hyp) :tag tag}))
            [:Only :Start :End :Middle])))
     symbol-hyps)))
