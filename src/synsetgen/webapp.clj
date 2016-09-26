(ns synsetgen.webapp
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [digest]
            [compojure.core :refer [defroutes GET POST ANY]]
            [compojure.route :refer [resources]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.multipart-params :refer [wrap-multipart-params]]
            [ring.middleware.session :refer [wrap-session]]
            [ring.middleware.cookies :refer [wrap-cookies]]
            [synsetgen.views :refer [termlist-submission-form
                                     display-termlist
                                     expanded-termlist-review-page
                                     term-cui-mapping-page
                                     synset-table-page
                                     synset-list-page
                                     filtered-termlist-view]]
            [synsetgen.process :refer [mirror-termlist
                                       process-termlist
                                       ;; process-termlist-and-termlistfile
                                       write-filtered-termlist
                                       process-filtered-synset]]))

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
  "set session var dataset in response...

  WebBrowser state variables set by this function:

  cookies:
     termtool-user username for session.
  sessioninfo:
     user  - same as termtool-user"
  ([cookies session]
   (if (:my-var session)
    {:body "Session variable already set"}
    {:body "Nothing in session, setting the var" 
     :session (assoc session :my-var "foo")}))
  ([cookies session body]
   (let [username (cond
                    (contains? cookies :termtool-user) (:termtool-user cookies)
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

  (GET "/" {cookies :cookies session :session}
    (set-session-username cookies session (termlist-submission-form "Input Terms")))
  
  (POST "/processtermlist/" {cookies :cookies session :sessions params :params }
    (let [{cmd "cmd" termlist "termlist"} params]
      (set-session-username
       cookies
       (assoc session :dataset (digest/sha-1 termlist)) ; add dataset key to session
       (case cmd
         "synset list"  (synset-list-page (process-termlist termlist)) ; primary 
         "test0"        (display-termlist (mirror-termlist termlist))
         "test1"        (expanded-termlist-review-page (process-termlist termlist))
         "term->cui"    (term-cui-mapping-page (process-termlist termlist))
         "synset table" (synset-table-page (process-termlist termlist))
         (expanded-termlist-review-page (process-termlist termlist)) ; default
         )
       )))

  (POST "/filtertermlist/" req
    (write-filtered-termlist req)
    (filtered-termlist-view req))

  (POST "/processfiltertermlist/" req
    (write-filtered-termlist req)
    (process-filtered-synset req)
    (filtered-termlist-view req))

  (GET "/sessioninfo/" req
    (str "request: <ul> <li>" (clojure.string/join "<li>" (mapv #(format "%s -> %s" (first %) (second %))
                                                            req))
         "</ul>"))
  
  (resources "/")
  )

(def app 
  (-> webroutes
      wrap-params
      wrap-multipart-params
      wrap-session
      wrap-cookies))



