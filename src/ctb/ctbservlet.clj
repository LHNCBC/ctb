(ns ctb.ctbservlet
 (:gen-class :extends javax.servlet.http.HttpServlet))

(def service-method)

(defn -service 
  [servlet request response]
  (if (nil? response)
    "<p>The data associated with session has expired.</p>"
    (service-method servlet request response)))

