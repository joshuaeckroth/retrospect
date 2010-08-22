(ns simulator.records
  (:import [java.io File])
  (:import [java.util Date])
  (:use [clojure.contrib.prxml :only (prxml)])
  (:use [clojure.xml :as xml :only (parse tag attrs)])
  (:use [clojure.zip :as zip :only (xml-zip node children)])
  (:use [clojure.contrib.zip-filter.xml :as zf :only (xml-> text attr=)])
  (:use [clojure.contrib.shell :only (sh)])
  (:use [clojure.java.io :as io :only (writer reader copy)])
  (:use [clojure.string :only (split)])
  (:use [simulator.runners.local :only (run-local)])
  (:use [simulator.charts :only (save-plots)])
  (:use [simulator.types.problem :only (get-headers)]))

(defn get-gitcommit []
  (first (split (sh "c:/progra~1/git/bin/git.exe" "rev-list" "HEAD") #"\n")))

(defn copy-params-file [destfile paramsfile] (io/copy (File. paramsfile) (File. destfile)))

(defn read-params
  [problem paramsfile]
  "Reads parameters from params XML file for a certain problem. Result
  is a map like {:SensorCoverage [0 10 20], :BeliefNoise [0 10 20]}"
  (let [xmltree (zip/xml-zip (xml/parse (File. paramsfile)))
	paramstree (zf/xml-> xmltree :problem (attr= :name (:name problem)) :params children)
	paramsmaps (apply merge (map (fn [p] {(first (zf/xml-> p tag))
					      (first (xml-> p attrs))}) paramstree))
	paramtags (keys paramsmaps)
	get-value (fn [pm p k] (Integer/parseInt (k (p pm))))
	update-with-range (fn [pm paramtag]
			    (assoc pm paramtag
				   (range (get-value paramsmaps paramtag :start)
					  (get-value paramsmaps paramtag :end)
					  (get-value paramsmaps paramtag :step))))]
    (reduce update-with-range {} paramtags)))

(defn explode-params
  [params]
  {:pre [(not (empty? params))]}
  "Want, e.g. {:Xyz [1 2 3], :Abc [3 4]} to become [{:Xyz 1, :Abc 3}, {:Xyz 2, :Abc 4}, ...]"
  (if (= 1 (count params))
    (for [v (second (first params))] {(first (first params)) v})
    (let [p (first params)
	  deeper (explode-params (rest params))]
      (flatten (map (fn [v] (map #(assoc % (first p) v) deeper)) (second p))))))

(defn write-xml [filename]
  (with-open [writer (io/writer filename)]
    (.write writer
	    (with-out-str
	      (prxml [:decl! {:version "1.0"}]
		     [:record
		      [:git-commit (get-gitcommit)]])))))

(defn run-with-new-record
  [problem paramsfile recordsdir nthreads]
  "Create a new folder for storing run data and execute the run. Then,
  depending on whether hadoop is true or false, execute a hadoop job
  control process or a local (this machine) runner."
  (let [dir (str recordsdir "/" (. System (currentTimeMillis)))
	params (explode-params (read-params problem paramsfile))]
    (println (format "Making new directory %s..." dir))
    (.mkdir (File. dir))
    (println "Writing meta.xml...")
    (write-xml (str dir "/meta.xml"))
    (println "Copying params file...")
    (copy-params-file (str dir "/params.xml") paramsfile)
    (run-local problem params dir nthreads)
    (println "Saving charts...")
    (save-plots dir)))

(defn write-input
  [params input]
  "This function only writes the input file."
  (with-open [writer (io/writer input)]
    (doseq [p params] (.write writer (str (print-str p) \newline)))))

(defn prepare-hadoop [problem paramsfile recordsdir]
  (write-input (explode-params (read-params problem paramsfile)) (str recordsdir "/input.txt")))

(defn cleanup-hadoop-results
  [problem dir file1 file2]
  (with-open [reader (io/reader file1)
	      writer (io/writer file2)]
    (.write writer (apply str (concat (interpose "," (map name (get-headers problem))))))
    (doseq [line (line-seq reader)]
      (.write writer (str (clojure.string/replace line #"^\d+\s+" "") \newline))))
  (println "Saving charts...")
  (save-plots dir))

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

