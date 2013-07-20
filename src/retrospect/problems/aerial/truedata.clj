(ns retrospect.problems.aerial.truedata
  (:require [clojure.string :as str])
  (:require [clojure.data.xml :as xml])
  (:require [clojure.zip :as zip])
  (:use [retrospect.state]))

(defn extract-object
  [object]
  (let [objzip (zip/xml-zip object)
        {:keys [xc yc w h]} (:attrs (zip/node (zip/down objzip)))
        {:keys [type r]} (:attrs (zip/node (zip/right (zip/down objzip))))]
    {:objid (:id (:attrs object))
     :xc (Double/parseDouble xc)
     :yc (Double/parseDouble yc)
     :w (Double/parseDouble w)
     :h (Double/parseDouble h)
     :r (Double/parseDouble r)}))

(defn frames-from-xml
  [folder]
  (let [tree (zip/xml-zip (xml/parse-str (slurp (format "%s/aerial/%s/%s.xml"
                                                        @datadir folder folder))))
        frames (vec (for [frame (zip/children tree)]
                      (let [objects (zip/children (zip/down (zip/xml-zip frame)))]
                        {:image (format "%s/aerial/%s/%s" @datadir folder
                                        (str/replace (:file (:attrs frame)) #".*\/" ""))
                         :objects (map extract-object objects)})))]
    ;; add timestamps to objects and create a time-keyed map of frames
    (reduce (fn [m t] (let [frame (nth frames t)]
                        (assoc m t (assoc frame :objects (map (fn [obj] (assoc obj :time t))
                                                              (:objects frame))))))
            {} (range (count frames)))))

(defn generate-truedata
  []
  (let [frames (frames-from-xml (:Folder params))]
    {:test frames
     :training {}}))
