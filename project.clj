(defproject starburst "0.1.0-SNAPSHOT"
  :description "An automatic generator of orion library"
  :url "https://github.com/idmit/starburst"
  :license {:name "MIT License"
            :url  "https://opensource.org/licenses/MITl"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha11"]
                 [cheshire "5.6.3"]]
  :main ^:skip-aot starburst.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
