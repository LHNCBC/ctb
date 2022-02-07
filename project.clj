(defproject ctb "0.1.2-SNAPSHOT"
  :description "term -> synsets generator"
  :url "http://ii.nlm.nih.gov/"
  :license {:name "Public Domain"
            :url "http://www.usa.gov/government-works"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/data.csv "0.1.4"]
                 [org.clojure/data.xml "0.0.8"]
                 [org.clojure/data.zip "1.0.0"]
                 [org.slf4j/slf4j-api "1.7.32"]
                 [org.slf4j/slf4j-nop "1.7.32"]
                 [org.clojure/tools.logging "1.2.3"]
                 [org.owasp.encoder/encoder-esapi "1.2.3"
                  :exclusions [org.owasp.esapi/esapi]]
                 [digest "1.4.4"]
                 [hiccup "1.0.5"]
                 [compojure "1.6.1"]
                 [ring/ring-devel "1.8.1"]
                 [ring/ring-core "1.8.1"]
                 [ring/ring-jetty-adapter "1.8.0"]
                 [ring-basic-authentication "1.0.5"]
                 [skr "0.1.2-SNAPSHOT"]
                 [irutils "2.0"]
                 [umls-tables "0.1.0-SNAPSHOT"]
                 [lvgclj "0.1.0-SNAPSHOT"]
                 [org.lpetit.ring/ring-java-servlet "0.2.0"]
                 [clj-time "0.15.2"]
                 ]
  :dev {:dependencies [[javax.servlet/servlet-api "2.5"]]}
  :plugins [[lein-ring "0.12.5"]]
  :ring {:handler ctb.webapp/app
         :web-xml "web.xml"}
  :repositories [["ii" "https://metamap.nlm.nih.gov/maven2/"]
                 ["jitpack" "https://jitpack.io"]]
  :marginalia {:javascript ["mathjax/MathJax.js?config=default"]}
  :min-lein-version "2.5.0"
  :target-path "target/%s"
  :jvm-opts ["-Dclojure.tools.logging.factory=clojure.tools.logging.impl/slf4j-factory"]
  :aot [ctb.core
        ctb.process-config
        ctb.ctblistener
        ctb.ctbservlet
        ctb.process
        ctb.views]
  :main ctb.core)
