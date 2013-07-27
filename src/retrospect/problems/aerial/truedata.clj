(ns retrospect.problems.aerial.truedata
  (:import (java.awt.image BufferedImage))
  (:import (javax.imageio ImageIO))
  (:use [clojure.java.io :only [file]])
  (:require [clojure.string :as str])
  (:require [clojure.data.xml :as xml])
  (:require [clojure.zip :as zip])
  (:use [retrospect.evaluate :only [avg]])
  (:use [retrospect.state]))

(defn extract-object
  [image object]
  (let [img (ImageIO/read (file image))
        objzip (zip/xml-zip object)
        box (zip/node (zip/down objzip))
        rep (zip/node (zip/right (zip/down objzip)))
        detscore (when-let [oz (zip/right (zip/right (zip/down objzip)))]
                   (zip/node oz))
        {:keys [xc yc w h]} (:attrs box)
        {:keys [type r]} (:attrs rep)
        {:keys [value]} (:attrs detscore)
        x (int (Double/parseDouble xc))
        y (int (Double/parseDouble yc))
        w (int (Double/parseDouble w))
        h (int (Double/parseDouble h))
        x0 (int (- x (/ w 2)))
        y0 (int (- y (/ h 2)))
        pixels (for [px (range x0 (+ x0 w)) py (range y0 (+ y0 h))]
                 (.getRGB img px py))]
    {:objid (:id (:attrs object))
     :x x :y y
     :detscore (Double/parseDouble (or value "0.0"))
     :avgpixel (avg pixels)}))

(defn frames-from-xml
  [folder test-or-truth]
  (let [fname (if (= "truth" test-or-truth)
                (format "%s/aerial/Training Data Sets/%s/%s.xml"
                        @datadir folder folder)
                (format "%s/aerial/IPF Detections/%s-D.xml"
                        @datadir folder))
        tree (zip/xml-zip (xml/parse-str (slurp fname)))
        frames (vec (for [frame (zip/children tree)]
                      (let [objects (zip/children (zip/down (zip/xml-zip frame)))
                            image (format "%s/aerial/Training Data Sets/%s/%s" @datadir folder
                                          (str/replace (:file (:attrs frame)) #".*\/" ""))]
                        {:image image
                         :objects (map (partial extract-object image) objects)})))]
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
