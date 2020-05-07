(ns ctb.ring-utils
  (:require [clojure.tools.logging :as log])
  (:import [javax.servlet ServletContext]))

(def TEMPDIR "javax.servlet.context.tempdir")

(defn get-context-attribute
  "get value of server context attribute"
  [request attribute-name]
  (let [^ServletContext servlet-context (:servlet-context request)]
    (log/info "type of servlet-context: " (type servlet-context))
    (log/info "attribute-name: " attribute-name)
    (when servlet-context
      (log/info "attributes in servlet-context:" (pr-str
                                                  (enumeration-seq
                                                   (.getAttributeNames servlet-context))))
      (.getAttribute servlet-context attribute-name))))
