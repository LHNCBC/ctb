(defproject synsetgen "0.1.0-SNAPSHOT"
  :description "term -> synsets generator"
  :url "http://ii.nlm.nih.gov/"
  :license {:name "Public Domain"
            :url "http://www.usa.gov/government-works"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/data.csv "0.1.2"]
                 [org.clojure/data.xml "0.0.8"]
                 [org.clojure/data.zip "0.1.1"]

                 [hiccup "1.0.5"]
                 [compojure "1.5.1"]
                 [ring/ring-core "1.5.0"]
                 [ring/ring-jetty-adapter "1.4.0"]
                 [skr "0.1.0-SNAPSHOT"]
                 [lexicalsystems/lvg "2016"]
                 [clj-wordnet "0.1.0"]
                 [irutils "2.0-SNAPSHOT"]
                 [umls-tables "0.1.0-SNAPSHOT"]]
  :plugins [[lein-ring "0.9.7"]]
  :ring {:handler synsetgen.webapp/app
         :init synsetgen.process/init}
  :marginalia {:javascript ["mathjax/MathJax.js?config=default"]}
  :min-lein-version "2.5.0"
  :aot [synsetgen.core]
  :main synsetgen.core)
