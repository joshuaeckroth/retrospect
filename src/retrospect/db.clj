(ns retrospect.db
  (:use [korma.db])
  (:use [korma.core])
  (:import (java.util Date))
  (:import (java.text SimpleDateFormat))
  (:require [clojure.contrib.string :as str])
  (:use [retrospect.state]))

(defn set-db
  [dbhost dbname dbuser dbpassword]
  (default-connection
    (create-db
     (mysql {:db dbname :user dbuser :password dbpassword :host dbhost}))))

(defentity parameters
  (pk :paramid))

(defentity analyses
  (pk :analysisid))

(defentity run-analyses
  (table :run_analyses)
  (pk :runanalysisid)
  (has-one analyses {:fk :analysisid}))

(defentity graphs
  (pk :graphid))

(defentity run-graphs
  (table :run_graphs)
  (pk :rungraphid)
  (has-one graphs {:fk :graphid}))

(defentity runs
  (pk :runid)
  (belongs-to parameters {:fk :paramid})
  (has-many run-graphs {:fk :runid})
  (has-many run-analyses {:fk :runid}))

(defentity table-fields
  (table :table_fields)
  (pk :tfid)
  (belongs-to runs {:fk :runid}))

(defentity simulations
  (pk :simid)
  (belongs-to runs {:fk :runid}))

(defentity results-fields
  (table :results_fields)
  (pk :rfid)
  (belongs-to simulations {:fk :simid}))

(defn commit-run
  [run-meta all-results]
  (transaction
   (let [runid (:generated_key (insert runs (values [run-meta])))
         _ (print (format "Sending %d results... " (count all-results)))]
     (when runid
       (doseq [sim-results all-results]
         (let [simid (:generated_key
                      (insert simulations
                              (values [{:runid runid
                                        :controlparams
                                        (or (:control-params
                                             (last (:control sim-results)))
                                            (:params (last (:control sim-results))))
                                        :comparisonparams
                                        (:comparison-params
                                         (last (:comparison sim-results)))}])))]
           (doseq [resultstype [:control :comparison :comparative]]
             (when (resultstype sim-results)
               (let [results (dissoc (last (resultstype sim-results))
                                     :control-params :comparison-params :params :simulation)]
                 (doseq [[field val] results]
                   (let [entry {:simid simid :resultstype (name resultstype)
                                :field (name field)}
                         entry-typed (cond (= Double (type val))
                                           (assoc entry :valtype "floatval" :floatval val)
                                           (= Integer (type val))
                                           (assoc entry :valtype "intval" :intval val)
                                           :else
                                           (assoc entry :valtype "strval" :strval val))]
                     (insert results-fields (values [entry-typed]))))))))))
     (println "done."))))

(defn get-params
  [problem name]
  (first (select parameters
                 (where {:name name :problem problem})
                 (order :rev :DESC) (limit 1))))

(defn read-params
  [params-string]
  (let [[problem name] (str/split #"/" params-string)
        params (get-params problem name)]
    (if-not params
      (do
        (println "No such parameters.")
        (System/exit -1))
      [problem (if (:comparison params)
                 (-> params
                     (update-in [:control] read-string)
                     (update-in [:comparison] read-string))
                 (update-in params [:control] read-string))])))

(defn format-date
  [ms]
  (.format (SimpleDateFormat. "yyyy-MM-dd HH:mm:ss") (Date. (long ms))))
