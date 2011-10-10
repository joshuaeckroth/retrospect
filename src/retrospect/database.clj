(ns retrospect.database
  (:require [com.ashafa.clutch :as clutch])
  (:require [clojure.contrib.string :as str])
  (:use [retrospect.state]))

(def active (atom nil))

(defn new-active
  [meta-info]
  (clutch/with-db @database
    (let [id (:_id (clutch/create-document (assoc meta-info :control [] :comparison [] :comparative [])))]
      (swap! active (constantly id)))))

(defn put-results-row
  [results-type results]
  (clutch/with-db @database
    (let [results-id (:_id (clutch/create-document (assoc results :runid @active :type results-type)))]
      (-> (clutch/get-document @active)
          (clutch/update-document #(conj % results-id) [results-type])))))

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
      (-> params
          (update-in [:control] read-string)
          (update-in [:comparison] read-string)
          (update-in [:player] read-string)))))

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
