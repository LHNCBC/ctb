(defproject ctb "0.1.2-SNAPSHOT"
  :description "term -> synsets generator"
  :url "http://ii.nlm.nih.gov/"
  :license {:name "Public Domain"
            :url "http://www.usa.gov/government-works"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/data.csv "0.1.4"]
                 [org.clojure/data.xml "0.0.8"]
                 [org.clojure/data.zip "0.1.2"]
                 [org.clojure/tools.logging "0.4.1"]
                 [org.slf4j/slf4j-log4j12 "1.8.0-beta4"]
                 [digest "1.4.4"]
                 [hiccup "1.0.5"]
                 [compojure "1.6.1"]
                 [ring/ring-devel "1.6.1"]
                 [ring/ring-core "1.6.1"]
                 [ring/ring-jetty-adapter "1.7.1"]
                 [ring-basic-authentication "1.0.5"]
                 [org.lpetit.ring/ring-java-servlet "0.2.0"]
                 [javax.servlet/servlet-api "2.5"]
                 [skr "0.1.2-SNAPSHOT"]
                 [irutils "2.0"]
                 [umls-tables "0.1.0-SNAPSHOT"]
                 [lvgclj "0.1.0-SNAPSHOT"]]
  :plugins [[lein-ring "0.12.4"]]
  :ring {:handler ctb.webapp/app
         :init ctb.process/init
         :listener-class org.lpetit.ring.servlet.RingServletContextListener}
  :repositories [["ii" "https://metamap.nlm.nih.gov/maven2/"]]
  :marginalia {:javascript ["mathjax/MathJax.js?config=default"]}
  :min-lein-version "2.5.0"
  :aot [ctb.core
        ctb.process-config]
  :main ctb.core)
