(ns retrospect.database
  (:require [com.ashafa.clutch :as clutch])
  (:require [clojure.contrib.string :as str])
  (:use [retrospect.state]))

(defn commit-sim
  [run rs results-type]
  (clutch/with-db @database
    (let [results-doc (clutch/create-document
                       {:type (format "%s" results-type)
                        :problem (:problem run)})
          results-ids (map :id (clutch/bulk-update
                                (map #(assoc % :runid (:_id run) :type results-type
                                             :resultsid (:_id results-doc)) rs)))]
      (clutch/update-document results-doc {:results results-ids})
      (:_id results-doc))))

(defn commit-results
  [run results results-type]
  (print (format "Sending %d results of type %s... "
                 (reduce + 0 (map count results)) results-type))
  (let [rs (doall (map #(commit-sim run % results-type) results))]
    (println "done.")
    rs))

(defn commit-run
  [run control comparison comparative]
  (clutch/with-db @database
    (let [run (clutch/create-document run)
          control-ids (commit-results run control "control")
          comparison-ids (commit-results run comparison "comparison")
          comparative-ids (commit-results run comparative "comparative")]
      (clutch/update-document run {:control control-ids
                                   :comparison comparison-ids
                                   :comparative comparative-ids}))))

(defn read-params
  [params-string]
  (let [[problem name] (str/split #"/" params-string)
        params (:value (first (:rows (clutch/with-db @database
                                       (clutch/get-view "app" "parameters-list"
                                                        {:key [problem name]})))))]
    (if-not params
      (do
        (println "No such parameters.")
        (System/exit -1))
      (if (= "comparative" (:paramstype params))
        (-> params
            (update-in [:control] read-string)
            (update-in [:comparison] read-string))
        (update-in params [:control] read-string)))))

(defn get-player-params
  [pname]
  (let [[problem name] (str/split #"/" pname)]
    (read-string
     (:player
      (first
       (map :value
            (:rows (clutch/with-db @database
                     (clutch/get-view "app" "parameters-list"
                                      {:key [problem name]})))))))))

(defn list-params
  []
  (sort
   (map #(format "%s/%s" (:problem %) (:name %))
        (filter #(= (:name @problem) (:problem %))
                (map :value
                     (:rows (clutch/with-db @database
                              (clutch/get-view "app" "parameters-list"))))))))
