(ns ctb.webapp
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :refer [trim index-of]]
            [clojure.tools.logging :as log]
            [digest]
            [compojure.core :refer [defroutes GET POST ANY]]
            [compojure.route :refer [files resources]]
            [ring.middleware.file :refer [wrap-file]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.basic-authentication :as basic]
            [ring.middleware.nested-params :refer [wrap-nested-params]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.multipart-params :refer [wrap-multipart-params]]
            [ring.middleware.session :refer [wrap-session]]
            [ring.middleware.session.cookie :refer [cookie-store]]
            [ring.middleware.cookies :refer [wrap-cookies]]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [ring.middleware.file :refer [wrap-file]]
            [ring.util.io :refer [piped-input-stream]]
            [ring.util.response :as r]
            [org.lpetit.ring.servlet.util :as util]
            [hiccup.util]
            [ctb.sessions :refer [wrap-expire-sessions]]
            [ctb.manage-datasets :refer [in-whitelist
                                         map-user-datasets
                                         map-user-dataset-filename]]
            [ctb.views :refer [termlist-submission-form
                               display-termlist
                               expanded-termlist-review-page
                               term-cui-mapping-page
                               synset-table-page
                               synset-list-page
                               filtered-termlist-view
                               display-error-message
                               display-dataset-list
                               user-error-message]]
            [ctb.ring-utils :refer [TEMPDIR get-context-attribute]]
            [ctb.process-config :refer [handle-servlet-context
                                        handle-servlet-context-path]]
            [ctb.process :refer [get-appcontext
                                 mirror-termlist                                 
                                 process-termlist
                                 ;; process-termlist-and-termlistfile
                                 write-filtered-termlist
                                 process-filtered-synset
                                 termlist-to-cui-concept-map
                                 cui-concept-map-to-mrconso-write
                                 cui-concept-map-to-mrconso-recordlist
                                 expand-cui-concept-map
                                 termlist-to-cuiset
                                 cuicoll-to-custom-mrsty-write
                                 get-servlet-context-tempdir
                                 stream-mrconso]])
  (:import (javax.servlet ServletContext)))

(defn modify-cookies
  "remove all cookies except termtool-user"
  [cookie-map]
  (assoc (select-keys cookie-map ["termtool-user"])
         :domain "nlm.nih.gov"
         :max-age 500
         :same-site :strict
         :secure true))

(defn modify-request
  [request]
  ;; (let [headers 
  (assoc request
         :cookies (modify-cookies (:cookies request))))

(defn wrap-user [handler]
  (fn [request]
    (if-let [user-id (-> request :cookies (get "termtool-user") :value)]
      (handler (assoc request :user user-id))
      (handler request))))

;; Determine if initialization has already occurred by checking
;; servlet context; if not then do any initialization and set state in
;; servlet context.
(defn wrap-context [handler]
  (fn [request]
    (get-appcontext request)
    (handler request)))

;; Get servlet context path and place it in request under
;; key: :servlet-context-path
(defn wrap-context-path [handler]
  (fn [request]
    (handle-servlet-context-path (:servlet-context-path request))
    (handler request)))

(defn wrap-exception-handling [handler]
  (fn [request]
    (try (handler request)
      (catch Exception e
         {:status 400
          :body "An Error occurred."}))))

;; # Current session and cookie information
;;
;; The cookie variable 'termtool-user' contains the current username
;;
;; The session variable 'user' also contains the current username,
;; taken from 'termtool-user' cookie.
;;
;; The session variable 'dataset' is currenty set using the sha-1
;; checksum of termlist supplied to the route POST /processtermlist/
;; by the 'Synset List HTML form.
;;
(defn set-session-username
  "Set session var dataset in response...

  WebBrowser state variables set by this function:

  cookies:
     termtool-user username for session
  sessioninfo:
     user  - same as termtool-user"
  ([cookies session]
   (if (:my-var session)
    {:body "Session variable already set"}
    {:body "Nothing in session, setting the var" 
     :session (assoc session :my-var "foo")}))
  ([cookies session body]
   (let [username (cond
                    (contains? cookies "termtool-user") (-> cookies (get "termtool-user") :value)
                    (contains? session :user) (:user session)
                    :else (str "user" (rand-int 100000)))]
     {:body body
      :cookies (assoc cookies :termtool-user username)
      :session (assoc session :user username)})))

  
;; # Web Applications (Routes)
;;
;; Primary URLs for Application
;;
;; * `/`                       `(GET)`  display initial form for input terms
;; * `/processtermlist/`       `(POST)` process input terms 
;; * `/filtertermlist/`        `(POST)` display expanded termlist form
;; * `/processfiltertermlist/` `(POST)` process expanded termlist using user's selections
(defroutes
  webroutes

  ;; (let [nrepl-handler (cemerick.drawbridge/ring-handler)]
  ;;   (ANY "/repl" request (nrepl-handler request)))

  (GET "/" {cookies :cookies session :session :as request}
    (get-appcontext request)
    (-> (set-session-username
         cookies session
         (termlist-submission-form request "Custom Taxonomy Builder"))
        (assoc-in [:headers "Content-Type"] "text/html")))
  
  (POST "/processtermlist/" {cookies :cookies session :sessions params :params :as request}
    (let [{cmd "cmd" rawtermlist "termlist" dataset "dataset"} params
          appcontext (get-appcontext request)
          termlist (hiccup.util/escape-html rawtermlist)]
      {:body
       (if (= (count (trim termlist)) 0)
         (user-error-message
          request
          "User Input Error: Termlist is Empty" "User Input Error: Termlist is empty.")
         (synset-list-page request (process-termlist appcontext dataset termlist)))
       :session (assoc session :dataset (digest/sha-1 termlist)) ; add dataset key to session
       :cookies cookies
       :headers {"Content-Type" "text/html"}}))

  (POST "/filtertermlist/" req
    (get-appcontext req)
    (write-filtered-termlist req)
    (->
     (filtered-termlist-view req)
     (assoc-in [:headers "Content-Type"] "text/html")))

  (POST "/processfiltertermlist/" req
    (get-appcontext req)
    (->
     {:body (do
              (write-filtered-termlist req)
              (process-filtered-synset req)
              (filtered-termlist-view req))
      :session (:session req)
      :cookies (:cookies req)}
     (assoc-in [:headers "Content-Type"] "text/html")))

  (GET "/sessioninfo/" req
    (get-appcontext req)
      {:body 
       (str "request: <ul> <li>"
            (clojure.string/join
             "<li>" (mapv #(format "%s -> %s" (first %) (second %))
                          req))
            "</ul>")
       :session (:session req)
       :cookies (:cookies req)
       :headers {"Content-Type" "text/html"}})
    

  (GET "/datasetsinfo/" {cookies :cookies session :session :as req}
    (get-appcontext req)
    {:body 
     (let [user (cond
                  (contains? session :user) (:user session)
                  (contains? cookies "termtool-user") (-> cookies (get "termtool-user") :value)
                  :else "NoUserName")]
       (if (and (index-of user "..")
                (= user "NoUserName"))
         (display-error-message req "Error: no username in session or cookie!")
         (let [^ServletContext servlet-context (:servlet-context req)
               workdir (if servlet-context
                         (get-servlet-context-tempdir servlet-context)
                         "resources/public/output")]
           (log/info "workdir: " workdir)
           (display-dataset-list req user (map-user-datasets workdir user)))))
     :session session
     :cookies cookies
     :headers {"Content-Type" "text/html"}})


  (GET "/dataset/:dataset/:filename" {{dataset :dataset
                                       filename :filename} :params
                                      cookies :cookies
                                      session :session
                                      :as request}
    (get-appcontext request)
    (let [user (cond
                 (contains? session :user) (:user session)
                 (contains? cookies "termtool-user") (-> cookies
                                                         (get "termtool-user")
                                                         :value)
                 :else "NoUserName")
          ^ServletContext servlet-context (:servlet-context request)
          workdir (if servlet-context
                    (get-servlet-context-tempdir servlet-context)
                    "resources/public/output")
          filepath (map-user-dataset-filename workdir user dataset filename)
          [body context-type]
          (cond
            (= user "NoUserName") (vector
                                   (display-error-message
                                    request "Error: session error with username!")
                                   {"Content-Type" "text/html"})
            
            (and (in-whitelist filename)
                 (.exists (io/file filepath))
                 (nil? (index-of filepath ".."))) (vector (slurp filepath)
                                                          {"Content-Type" "text/plain"})
            
            :else (vector (display-error-message
                           request
                           (str "File: " filename "(" filepath ") does not exist."))
                          {"Content-Type" "text/html"}))]
      {:body body
       :session session
       :cookies cookies
       :headers context-type}))
  
  ;; given a termlist in POST request skip filter step and go directly
  ;; to mrconso generation.
  ;; Note: we need to use piped-input-stream to avoid running out of memory.
  (POST "/rest/mrconso" {params :params :as request}
    (get-appcontext request)
    ;; (let [{termlist "termlist"} params
    ;;       cui-concept-map (expand-cui-concept-map (termlist-to-cui-concept-map termlist))
    ;;       stream-mrconso (fn [out]
    ;;                        (dorun
    ;;                         (map #(.write out %)
    ;;                              (cui-concept-map-to-mrconso-recordlist cui-concept-map))))]
    ;;   (piped-input-stream #(stream-mrconso (io/make-writer % {})))))
    (stream-mrconso params)
    )
  
  ;; given a termlist in POST request skip filter step and go directly
  ;; to mrsty generation.
  (POST "/rest/mrsty"  {params :params :as request}
    (get-appcontext request)
    (let [{termlist "termlist"} params
          cuiset (termlist-to-cuiset termlist)
          stream-mrsty (fn [out]
                         (cuicoll-to-custom-mrsty-write out cuiset))]
      (piped-input-stream #(stream-mrsty (io/make-writer % {})))
      ))

  (files "/" {:mime-types {"rrf" "text/plain"}})  
  (resources "/" {:mime-types {"rrf" "text/plain"}})
  (ANY "/*" req (display-error-message req "Invalid URL!"))
)

(def app 
  (-> webroutes
      wrap-nested-params
      wrap-keyword-params
      wrap-params
      wrap-multipart-params
      wrap-cookies
      wrap-session
      wrap-user
      wrap-expire-sessions
      wrap-exception-handling
      ))
