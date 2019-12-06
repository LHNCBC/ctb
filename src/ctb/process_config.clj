(ns ctb.process-config
  (:require [ctb.process :refer [init]]
            [clojure.tools.logging :as log]
            [org.lpetit.ring.servlet.util :as util])
  (:import (javax.servlet ServletContext)))

(defn handle-servlet-context
   "Check servlet context if application context not
  initialized (attribute 'ctbappcontext') then initialize application
  context."
  [context]
  (log/debug "context: " context)
  (when-not (nil? context)
    (when (nil? (.getAttribute context "ctbappcontext"))
      (let [root-path (.getRealPath context "/")
            data-path (.getRealPath context "/data/")
            config-path (.getRealPath context "/config/")]
        (log/debug "ctb.process/handle-servler-context: root-path: " root-path )
        (log/debug "ctb.process/handle-servler-context: data-path: " data-path )
        (log/debug "ctb.process/handle-servler-context: config-path: " config-path )
        (.setAttribute context "ctbappcontext" (init root-path data-path config-path))))
      context))

(defn handle-servlet-context-path
  [context-path]
  (log/debug "context-path: " context-path))

(defn on-startup
  [context]
  (log/debug "Starting app with params: " (util/context-params context))
  (let [servlet-context-map (util/context-params context)]
    (log/debug (format "ctb.process-config: util/context-params: %s" servlet-context-map))
  (if (nil? context)
    ;; running using jetty?
    (do
      (log/debug "context is nil.")
      (let [root-path ""
            data-path ""
            config-path "config"]
        (init root-path data-path config-path)))
    (handle-servlet-context context))))

(defn on-shutdown
  [context]
  (log/debug "Stopping app with params: " (util/context-params context)))


