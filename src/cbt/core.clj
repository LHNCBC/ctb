(ns cbt.core
  (:require [clojure.edn :as edn]
            [ring.adapter.jetty :refer [run-jetty]]
            [cbt.webapp :refer [app]])
  (:gen-class))


(defn -main
  "Print hello and launch Jetty with web app."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!")
  (if (> (count args) 0)
    (let [arg (edn/read-string (nth args 0))]
      (if (number? arg)
        (run-jetty app {:port arg :join false})
        (do
          (println (format "supplied argument is not a number!  arg: %s" arg))
          (println "usage: app port-number"))))
    (run-jetty app {:port 3000 :join false})))
