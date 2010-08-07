(ns simulator.records
  (:import [java.io File BufferedWriter FileWriter])
  (:import [java.util Date])
  (:use [clojure.contrib.prxml :only (prxml)])
  (:use [clojure.xml :as xml :only (parse)])
  (:use [clojure.zip :as zip :only (xml-zip node children)])
  (:use [clojure.contrib.zip-filter.xml :as zf :only (xml-> text)])
  (:use [clojure.contrib.shell :only (sh)])
  (:use [clojure.string :only (split)])
  (:use [simulator.types.parameters :only (get-headers get-params to-xml)])
  (:use [simulator.runner :only (save-results multiple-runs)])
  (:use [simulator.charts :only (save-plots)]))

(defn get-gitcommit []
  (first (split (sh "c:/progra~1/git/bin/git.exe" "rev-list" "HEAD") #"\n")))

(defn write-xml [filename params]
  (with-open [writer (BufferedWriter. (FileWriter. filename))]
    (.write writer
	    (with-out-str
	      (prxml [:decl! {:version "1.0"}]
		     [:record
		      [:git-commit (get-gitcommit)]
		      (to-xml params)])))))

(defn run-with-new-record
  [recordsdir params runner nthreads]
  (let [dir (str recordsdir "/" (. System (currentTimeMillis)))
	ps (get-params params)]
    (println (format "Making new directory %s" dir))
    (.mkdir (File. dir))
    (println "Writing meta.xml")
    (write-xml (str dir "/meta.xml") params)
    (println (format "Running %d simulations..." (count ps)))
    (save-results (str dir "/results.csv") (get-headers params) (multiple-runs ps runner nthreads))
    (println "Saving charts...")
    (save-plots dir)))

(defn record-str
  [id date commit params]
  (str
   (format "%s (%s)\n" id date)
   (format "  Commit: %s\n" (apply str commit))
   "  Params:\n"
   (apply str (for [p params] (format "    %s\n" p)))
   "\n"))

(defn params-str
  [xml]
  (zf/xml-> xml zip/children #(format "%s: %s" (name (:tag %)) (str (first (:content %))))))

(defn list-records
  [recordsdir]
  (apply println
   (let [records (filter #(.isDirectory %) (.listFiles (File. recordsdir)))]
     (for [r (sort-by #(.toString %) records)]
       (let [meta (zip/xml-zip (xml/parse (File. r "meta.xml")))
	     id (.getName r)
	     date (.toString (Date. (Long/parseLong id)))
	     commit (zf/xml-> meta :git-commit zf/text)
	     params (zf/xml-> meta :params params-str)]
	 (record-str id date commit params))))))