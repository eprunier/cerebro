(defproject cerebro "0.1.0-SNAPSHOT"
  :description "Linear algebra library for machine learning in Clojure"
  :url "http://github.com/eprunier/cerebro"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/data.csv "0.1.2"]
                 [net.mikera/core.matrix "0.19.0"]
                 [com.googlecode.efficient-java-matrix-library/ejml "0.24"]]
  :profiles {:dev {:dependencies [[org.clojure/tools.namespace "0.2.4"]]
                   :source-paths ["dev"]}}
  :codox {:exclude "cerebro.matrix-api"})
