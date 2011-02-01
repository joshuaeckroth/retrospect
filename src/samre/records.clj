(ns samre.records
  (:import [java.io File])
  (:import [java.util Date])
  (:use [clojure.contrib.prxml :only (prxml)])
  (:use [clojure.xml :as xml :only (parse tag attrs)])
  (:use [clojure.zip :as zip :only (xml-zip node children)])
  (:use [clojure.contrib.zip-filter.xml :as zf :only (xml-> text attr=)])
  (:use [clojure.contrib.shell :only (sh)])
  (:use [clojure.java.io :as io :only (writer reader copy)])
  (:use [clojure.string :only (split)])
  (:use [samre.runners.local :only (run-local)])
  (:use [samre.problem :only (get-headers)])
  (:use [samre.charts :only (save-plots)]))

(defn get-gitcommit []
  (first (split (sh "git" "rev-list" "HEAD") #"\n")))

(defn copy-params-file [destfile paramsfile] (io/copy (File. paramsfile) (File. destfile)))

(defn read-params
  [problem paramsfile]
  "Reads parameters from params XML file for a certain problem. Result
  is a map like {:SensorCoverage [0 10 20], :BeliefNoise [0 10 20]}"
  (let [xmltree (zip/xml-zip (xml/parse (File. paramsfile)))
	probtree (zf/xml-> xmltree :problems :problem (attr= :name (:name problem))
			     :params children)
	probmaps (apply merge (map (fn [p] {(first (zf/xml-> p tag))
                                            (first (xml-> p attrs))}) probtree))
	probtags (keys probmaps)
	get-value (fn [pm p k] (Integer/parseInt (k (p pm))))
	update-with-range (fn [pm probtag]
			    (assoc pm probtag
				   (range (get-value probmaps probtag :start)
					  (inc (get-value probmaps probtag :end))
					  (get-value probmaps probtag :step))))]
    (reduce update-with-range {} probtags)))

(defn explode-params
  "Want {:Xyz [1 2 3], :Abc [3 4]} to become [{:Xyz 1, :Abc 3}, {:Xyz 2, :Abc 4}, ...]"
  [params]
  {:pre [(not (empty? params))]}
  (if (= 1 (count params))
    (for [v (second (first params))]
      {(first (first params)) v})
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
  [problem paramsfile recordsdir nthreads monitor]
  "Create a new folder for storing run data and execute the run. Then,
  depending on whether hadoop is true or false, execute a hadoop job
  control process or a local (this machine) runner."
  (let [dir (str recordsdir "/" (. System (currentTimeMillis)))
	params (explode-params (read-params problem paramsfile))]
    (print (format "Making new directory %s..." dir))
    (.mkdir (File. dir))
    (println "done.")
    (print "Writing meta.xml...")
    (write-xml (str dir "/meta.xml"))
    (println "done.")
    (print "Copying params file...")
    (copy-params-file (str dir "/params.xml") paramsfile)
    (println "done.")
    (println (format "Running %d parameter combinations..." (count params)))
    (run-local problem params dir nthreads monitor)
    (println "Done.")))

(defn write-input
  [params input]
  "This function only writes the input file."
  (with-open [writer (io/writer input)]
    (doseq [p params] (.write writer (str (print-str p) \newline)))))

(defn prepare-hadoop [problem paramsfile recordsdir]
  (write-input (explode-params (read-params problem paramsfile))
	       (str recordsdir "/input.txt")))

(defn cleanup-hadoop-results
  [problem dir file1 file2]
  (with-open [reader (io/reader file1)
	      writer (io/writer file2)]
    (.write writer (apply str (concat (interpose "," (map name (get-headers problem))))))
    (doseq [line (line-seq reader)]
      (.write writer (str (clojure.string/replace line #"^\d+\s+" "") \newline)))))

(defn record-str
  [id date commit params]
  (str
   (format "%s (%s)\n" id date)
   (format "  Commit: https://github.com/joshuaeckroth/Simulator/commit/%s\n"
	   (apply str commit))
   "  Params:\n"
   (apply str (for [p params] (format "    %s\n" p)))
   "\n"))

(defn params-str
  [xml]
  (zf/xml-> xml zip/children #(format "%s: %s" (name (:tag %))
				      (str (first (:content %))))))

(defn list-records
  [recordsdir]
  (apply println
   (let [records (filter #(.isDirectory %) (.listFiles (File. recordsdir)))]
     (for [r (sort-by #(.toString %) records)]
       (try
	 (let [meta (zip/xml-zip (xml/parse (File. r "meta.xml")))
	       id (.getName r)
	       date (.toString (Date. (Long/parseLong id)))
	       commit (zf/xml-> meta :git-commit zf/text)
	       params (zf/xml-> (zip/xml-zip (xml/parse (File. r "params.xml")))
				:params params-str)]
	   (record-str id date commit params))
	 (catch Exception e))))))

(defn chart
  [recordsdir record problem]
  (print "Saving charts...")
  (save-plots (str recordsdir "/" record) problem)
  (println "done."))
