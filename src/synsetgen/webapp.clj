(ns synsetgen.webapp
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [compojure.core :refer [defroutes GET POST]]
            [compojure.route :refer [resources]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.multipart-params :refer [wrap-multipart-params]]
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


;; # Web Applications (Routes)
;;
;; Primary URLs for Application
;;
;; * `/`                       `(GET)`  display initial form for input terms
;; * `/processtermlist/`       `(POST)` process input terms 
;; * `/filtertermlist/`        `(POST)` display expanded termlist form
;; * `/processfiltertermlist/` `(POST)` process expanded termlist using user's selections
;;
(defroutes
  webroutes
  (GET "/" []
    (termlist-submission-form "Input Terms"))
  
  (POST "/processtermlist/" [termlist termlistfile cmd]
     (case cmd
       "synset list"  (synset-list-page
                       (process-termlist termlist))  ;; primary 
       "test0"        (display-termlist (mirror-termlist termlist)) ;; debugging
       "test1"        (expanded-termlist-review-page (process-termlist termlist)) ;; debugging
       "term->cui"    (term-cui-mapping-page (process-termlist termlist)) ;; debugging
       "synset table" (synset-table-page (process-termlist termlist)) ;; debugging
       (expanded-termlist-review-page (process-termlist termlist)) ; default
       ))

  (POST "/filtertermlist/" req
    (write-filtered-termlist req)
    (filtered-termlist-view req))

  (POST "/processfiltertermlist/" req
    (write-filtered-termlist req)
    (process-filtered-synset req)
    (filtered-termlist-view req))
  
  (resources "/")
  )

(def app 
  (-> webroutes
      wrap-params
      wrap-multipart-params))



