(ns simulator.records
  (:import [java.io File BufferedWriter FileWriter])
  (:use [clojure.contrib.prxml :only (prxml)])
  (:use [clojure.contrib.shell :only (sh)])
  (:use [clojure.string :only (split)])
  (:use [simulator.types.parameters :only (getHeaders getParams toXml)])
  (:use [simulator.runner :only (save-results multiple-runs)]))

(defn get-gitcommit []
  (first (split (sh "c:/progra~1/git/bin/git.exe" "rev-list" "HEAD") #"\n")))

(defn xml [filename params]
  (with-open [writer (BufferedWriter. (FileWriter. filename))]
    (.write writer
	    (with-out-str
	      (prxml [:decl! {:version "1.0"}]
		     [:record
		      [:git-commit (get-gitcommit)]
		      (toXml params)])))))

(defn run-with-new-record
  [recordsdir params runner]
  (let [dir (str recordsdir "/" (. System (currentTimeMillis)))
	ps (getParams params)]
    (println (format "Making new directory %s" dir))
    (.mkdir (File. dir))
    (println "Writing meta.xml")
    (xml (str dir "/meta.xml") params)
    (println (format "Running %d simulations..." (count ps)))
    (save-results (str dir "/results.csv") (getHeaders params) (multiple-runs ps runner))))