(ns ctb.ring-utils
  (:require [clojure.tools.logging :as log])
  (:import [javax.servlet ServletContext]))

(defn get-context-attribute
  "get value of server context attribute"
  [request attribute-name]
  (let [servlet-context (:servlet-context request)]
    (log/info "type of servlet-context: " (type servlet-context))
    (log/info "attribute-name: " attribute-name)
    (log/info "attributes in servlet-context:" (pr-str (enumeration-seq (.getAttributeNames servlet-context))))
    (.getAttribute ^ServletContext servlet-context attribute-name)))

