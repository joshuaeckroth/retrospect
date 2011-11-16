(ns retrospect.database
  (:require [com.ashafa.clutch :as clutch])
  (:require [clojure.contrib.string :as str])
  (:use [retrospect.state]))

(defn commit-run
  [run-meta results]
  (clutch/with-db @database
    (let [run (clutch/create-document
               (assoc run-meta :endtime (. System (currentTimeMillis))))
          _ (print (format "Sending %d results... " (count results)))
          results-runids (map #(assoc % :runid (:_id run)
                                      :problem (:problem run)
                                      :paramstype (:paramstype run)
                                      :simulation (:simulation (first (:control %)))
                                      :type "simulation")
                              results)
          results-ids (doall (map :id (clutch/bulk-update results-runids)))]
      (clutch/update-document run {:results results-ids})
      (println "done."))))

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
