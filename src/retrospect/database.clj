(ns retrospect.database
  (:require [com.ashafa.clutch :as clutch])
  (:require [clojure.contrib.string :as str])
  (:use [retrospect.state]))

(defn commit-run
  [run control comparison comparative]
  (clutch/with-db @database
    (let [rundoc (clutch/create-document run)
          runid (:_id rundoc)
          control-ids (map :id (clutch/bulk-update
                                (map #(assoc % :runid runid :type "control")
                                     control)))
          comparison-ids (map :id (clutch/bulk-update
                                   (map #(assoc % :runid runid :type "comparison")
                                        comparison)))
          comparative-ids (map :id (clutch/bulk-update
                                    (map #(assoc % :runid runid :type "comparative")
                                         comparative)))]
      (clutch/update-document rundoc
                              {:control control-ids
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
