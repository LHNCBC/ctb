(ns ctb.process-config
  (:require [org.lpetit.ring.servlet.util :as util]
            [ctb.process :refer [set-context-root-path
                                 set-context-data-path
                                 set-context-config-path
                                 init]]
            [clojure.tools.logging :as log])
  (:import (javax.servlet ServletContext)))

(defn on-startup
  [context]
  (let [servlet-context-map (util/context-params context)]
    (log/info (format "ctb.process-config: util/context-params: %s" servlet-context-map))
  (if (nil? context)
    ;; running using jetty 
    (let [root-path ""
          data-path  ""
          config-path  "config"]
      (init root-path data-path config-path))
    (when (nil? (.getAttribute context "ctbappcontext"))
      (let [servlet-context (util/context-params context)
            root-path (.getRealPath context "")
            data-path (.getRealPath context "")
            config-path (.getRealPath context "config")]
        (log/info "ctb.process/handle-servler-context: data-path: " data-path )
        (.setAttribute context "ctbappcontext" (init root-path data-path config-path)))))))

(defn on-shutdown
  [context]
  )
