(ns ctb.process-config
  (:require [org.lpetit.ring.servlet.util :as util]
            [ctb.process :refer [set-context-root-path
                                 set-context-data-path
                                 set-context-config-path
                                 init]]
            [clojure.tools.logging :as log]))

(defn on-startup
  [context]
  (let [servlet-context (util/context-params context)]
    (log/info (format "ctb.process-config: util/context-params: %s" servlet-context))
    (set-context-root-path (.getRealPath servlet-context "/"))
    (set-context-data-path (.getRealPath servlet-context "data"))
    (set-context-config-path (.getRealPath servlet-context "config"))
    (init)))

(defn on-shutdown
  [context]
  )
