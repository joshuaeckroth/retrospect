(ns retrospect.problems.aerial.truedata
  (:require [clojure.string :as str])
  (:require [clojure.data.xml :as xml])
  (:require [clojure.zip :as zip])
  (:use [retrospect.state]))

(defn extract-object
  [object]
  (let [objzip (zip/xml-zip object)
        box (zip/node (zip/down objzip))
        rep (zip/node (zip/right (zip/down objzip)))
        detscore (when-let [oz (zip/right (zip/right (zip/down objzip)))]
                   (zip/node oz))
        {:keys [xc yc w h]} (:attrs box)
        {:keys [type r]} (:attrs rep)
        {:keys [value]} (:attrs detscore)]
    {:objid (:id (:attrs object))
     :x (Double/parseDouble xc)
     :y (Double/parseDouble yc)
     :w (Double/parseDouble w)
     :h (Double/parseDouble h)
     :r (Double/parseDouble r)
     :detscore (Double/parseDouble (or value "0.0"))}))

(defn frames-from-xml
  [folder test-or-truth]
  (let [fname (if (= "truth" test-or-truth)
                (format "%s/aerial/Training Data Sets/%s/%s.xml"
                        @datadir folder folder)
                (format "%s/aerial/IPF Detections/%s-D.xml"
                        @datadir folder))
        tree (zip/xml-zip (xml/parse-str (slurp fname)))
        frames (vec (for [frame (zip/children tree)]
                      (let [objects (zip/children (zip/down (zip/xml-zip frame)))]
                        {:image (format "%s/aerial/Training Data Sets/%s/%s" @datadir folder folder
                                        (str/replace (:file (:attrs frame)) #".*\/" ""))
                         :objects (map extract-object objects)})))]
    ;; add timestamps to objects and create a time-keyed map of frames
    (reduce (fn [m t] (let [frame (nth frames t)]
                        (assoc m t (assoc frame :objects (map (fn [obj] (assoc obj :time t))
                                                              (:objects frame))))))
            {} (range (count frames)))))

(defn generate-truedata
  []
  (let [frames-test (frames-from-xml (:Folder params) "test")
        frames-truth (frames-from-xml (:Folder params) "truth")]
    {:test frames-test
     :truth frames-truth
     :training {}}))
