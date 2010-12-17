(defproject simulator "1.0.0-SNAPSHOT"
  :description "FIXME: write"
  :dependencies [[org.clojure/clojure "1.2.0-master-SNAPSHOT"]
                 [org.clojure/clojure-contrib "1.2.0-SNAPSHOT"]
		 [incanter "1.2.3-SNAPSHOT"]
		 [org.clojars.choas/clojure-hadoop "1.2.1-SNAPSHOT"]
                 [org.clojars.overtone/vijual "0.2.1"]]
  :dev-dependencies [[swank-clojure "1.2.1"] [org.clojars.rayne/autodoc "0.8.0-SNAPSHOT"]]
  :autodoc { :name "SIMULATOR", :page-title "SIMULATOR API Documentation"}
  :main simulator.core
  :source-path "src/main/java"
  :test-path "src/test/java"
  :library-path "target/dependency"
  :resources-path "resources"
  :target-dir "target"
  :keep-non-project-classes true)
