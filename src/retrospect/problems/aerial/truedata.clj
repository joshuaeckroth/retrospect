(ns retrospect.problems.aerial.truedata
  (:import (java.awt.image BufferedImage))
  (:import (javax.imageio ImageIO))
  (:use [clojure.java.io :only [file]])
  (:require [clojure.string :as str])
  (:require [clojure.data.xml :as xml])
  (:require [clojure.zip :as zip])
  (:use [geppetto.random])
  (:use [retrospect.evaluate :only [avg]])
  (:use [retrospect.state]))

(defn dist
  [x1 y1 x2 y2]
  (double (Math/sqrt (+ (* (- x1 x2) (- x1 x2))
                        (* (- y1 y2) (- y1 y2))))))

(defn near?
  [x1 x2]
  (< (Math/abs (- x1 x2)) 2.0))

(defn objects-near?
  [obj1 obj2]
  (and (near? (:x obj1) (:x obj2))
       (near? (:y obj1) (:y obj2))))

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
        x (Double/parseDouble xc)
        y (Double/parseDouble yc)
        w (int (Double/parseDouble w))
        h (int (Double/parseDouble h))
        x0 (int (- (int x) (/ w 2)))
        y0 (int (- (int y) (/ h 2)))
        pixels (try (doall (for [px (range x0 (+ x0 w)) py (range y0 (+ y0 h))]
                             (.getRGB img px py)))
                    (catch Exception e []))]
    {:objid (:id (:attrs object))
     :x x :y y
     :detscore (Double/parseDouble (or value "0.0"))
     :avgpixel (avg pixels)}))

(defn add-obj-metadata
  [obj t frames-truth]
  ;; note that a noisy det might get a random objid used by a true det
  (let [objid (or (if frames-truth
                    (:objid (first (filter #(objects-near? obj %)
                                           (:objects (get frames-truth t)))))
                    (:objid obj))
                  (when (<= (my-rand) (/ (:SensorDuplicationNoise params) 100.0))
                    (let [near-objects (take 5 (sort-by (fn [obj2] (dist (:x obj) (:x obj2)
                                                                         (:y obj) (:y obj2)))
                                                        (:objects (get frames-truth t))))]
                      (my-rand-nth (map :objid near-objects)))))]
    (assoc obj :time t :objid objid)))

(defn frames-from-xml
  [folder frames-truth]
  (let [fname (if frames-truth ;; loading testing set
                (format "%s/aerial/IPF Detections/%s-D.xml"
                        @datadir folder)
                (format "%s/aerial/Training Data Sets/%s/%s.xml"
                        @datadir folder folder))
        tree (zip/xml-zip (xml/parse-str (slurp fname)))
        frames (vec (for [frame (zip/children tree)]
                      (let [objects (zip/children (zip/down (zip/xml-zip frame)))
                            image (format "%s/aerial/Training Data Sets/%s/%s" @datadir folder
                                          (str/replace (:file (:attrs frame)) #".*\/" ""))]
                        {:image image
                         :objects (doall (map (partial extract-object image) objects))})))]
    ;; add objids and timestamps to objects and create a time-keyed map of frames
    (reduce (fn [m t] (let [frame (nth frames t)
                            ;; doall is important so that the rand calls all happen ahead of time
                            objs (doall (map #(add-obj-metadata % t frames-truth)
                                             (:objects frame)))]
                        (assoc m t (assoc frame :objects objs))))
            {} (range (count frames)))))

(defn find-movements
  [frames]
  (let [framesvec (vec (map second (sort-by first (seq frames))))]
    (doall (apply concat
                  (for [framenum (range (dec (count framesvec)))]
                    (let [thisframe (nth framesvec framenum)
                          nextframe (nth framesvec (inc framenum))]
                      (mapcat
                       (fn [object]
                         (let [objid (:objid object)
                               object2-options (filter #(= objid (:objid %)) (:objects nextframe))]
                           (for [object2 object2-options]
                             {:ox (:x object) :oy (:y object) :ot (:time object)
                              :x (:x object2) :y (:y object2) :time (:time object2)})))
                       (:objects thisframe))))))))

(defn generate-truedata
  []
  (let [frames-truth (frames-from-xml (:Folder params) nil)
        frames-test (frames-from-xml (:Folder params) frames-truth)
        frames-count (count frames-test)
        train-start (- frames-count (:TrainingCount params))
        frames-train (select-keys frames-truth (range train-start frames-count))
        frames-test-subset (select-keys frames-test (range train-start))
        frames-truth-subset (select-keys frames-truth (range train-start))]
    {:test frames-test-subset
     :truth frames-truth-subset
     :all-moves (find-movements frames-truth-subset)
     :training {:frames frames-train
                :all-moves (find-movements frames-train)}}))
