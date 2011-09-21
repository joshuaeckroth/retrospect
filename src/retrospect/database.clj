(ns retrospect.database
  (:require [com.ashafa.clutch :as clutch]))

(def local-couchdb "http://localhost:5984/retrospect")

(def active (atom nil))

(defn new-active
  [meta-info]
  (clutch/with-db local-couchdb
    (let [id (:_id (clutch/create-document (assoc meta-info :control [] :comparison [] :comparative [])))]
      (swap! active (constantly id)))))

(defn put-results-row
  [results-type results]
  (try
   (clutch/with-db local-couchdb
     (let [results-id (:_id (clutch/create-document (assoc results :runid @active :type results-type)))]
       (-> (clutch/get-document @active)
           (clutch/update-document #(conj % results-id) [results-type]))))
   (catch java.io.IOException e
     (put-results-row results-type results))))

