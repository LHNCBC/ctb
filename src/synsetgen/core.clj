(ns synsetgen.core
  (:require [ring.adapter.jetty :refer [run-jetty]]
            [synsetgen.webapp :refer [app]])
  (:gen-class))

(defn -main
  "Print hello and launch Jetty with web app."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!")
  (run-jetty app {:port 3000}))
