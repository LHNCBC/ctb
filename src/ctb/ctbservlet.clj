(ns ctb.ctbservlet
 (:gen-class :extends javax.servlet.http.HttpServlet))

(def service-method)

(defn -service 
  [servlet request response] 
  (service-method servlet request response))

