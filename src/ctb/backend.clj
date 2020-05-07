(ns ctb.backend
 (:require [clojure.string :refer [join split trim lower-case]]
            [clojure.java.io :as io]
            [clojure.set :refer [union intersection difference]]
            [clojure.tools.logging :as log]
            [ring.util.io :refer [piped-input-stream]]
            [digest]
            [ctb.umls-indexed :as umls-indexed]
            [ctb.synsetgen :as synset]
            [ctb.umls-indexed :refer [init-index]]
            [ctb.keylistexpansion :refer [init-lvg]]
            [ctb.ring-utils :refer [TEMPDIR get-context-attribute]])
  (:import (java.lang.Boolean)
           (java.util Properties)
           (javax.servlet ServletContext)
           (java.io File Reader Writer))
  (:gen-class))

;; # Backend Processing Functions

(defn init
  ([root-path data-path config-path]
   (log/info "ctb.backend/init: root-path: " root-path
             ", data-path: " data-path
             ", config-path: " config-path)
   ;; Load CBT properties from config/ctb.properties (settable by system
   ;; property "ctb.property.file")  (should this be in ctb.webapp/init?)
   (let [config-file-path (str config-path "/ctb.properties")
         config-properties (if (.exists (io/file config-file-path))
                             (doto (Properties.)
                               (.load ^Reader (io/reader config-file-path)))
                             (do (log/error
                                  (format "ctb.backend/init: config file %s does not exist."
                                          config-file-path))
                                 nil))
         ivf-dataroot (str root-path "/" (.getProperty config-properties "ctb.ivf.dataroot"))
         lvg-path (.getProperty config-properties "ctb.lvg.directory")
         lvg-directory (str root-path lvg-path)
         hide-vocab-sources ^boolean (Boolean/parseBoolean
                                      (.getProperty config-properties
                                                    "ctb.hide.vocab.sources"))
         ;; inverted file initialization
         ivf-indexes  (if (.exists (io/file ivf-dataroot))
                        (init-index ivf-dataroot "tables" "ifindices")
                        (do (log/error
                             (format "ctb.backend/init: data root file %s does not exist!"
                                     ivf-dataroot))
                            { "error" (str "error couldn't load indexes at " ivf-dataroot) }))
         ;; lvg initialization
         lvg-api (when (not (nil? lvg-path))
                   (if (.exists (io/file lvg-directory))
                     (init-lvg lvg-directory)
                     (do 
                       (log/error
                        (format "ctb.process/init: lvg directory %s does not exist."
                                lvg-directory))
                       nil)))
         newappcontext (hash-map :config-file-path config-file-path
                                 :config-properties config-properties
                                 :ivf-dataroot ivf-dataroot
                                 :ivf-indexes ivf-indexes
                                 :lvg-path lvg-path
                                 :lvg-initialized (not (nil? lvg-api))
                                 :lvg-api lvg-api
                                 :hide-vocab-sources hide-vocab-sources)]
     (log/debug (format "ctb.ivf.dataroot: %s" ivf-dataroot))
     (log/debug (format "ctb.lvg.directory: %s" lvg-directory))
     (log/debug (str "hide-vocab-sources: " hide-vocab-sources))
     (log/debug (format "lvg-api: %s" lvg-api))
     ;; tell synsetgen if vocabulary source abbreviation should be obscured.
     (synset/set-hide-vocab-sources! hide-vocab-sources)
     newappcontext))
  ([servlet-context]
   (let [real-path (.getRealPath servlet-context "")
         data-real-path (.getRealPath servlet-context "data")
         config-real-path (.getRealPath servlet-context "config")]
     (log/info (str "real-path: " real-path))
     (init real-path data-real-path config-real-path)))
  )


(defn init-using-servlet-context
    "Initialize any needed resources, resource are returned as map to caller."
  [context]
   (let [root-path (.getRealPath ^ServletContext context "/")
         data-path (.getRealPath ^ServletContext context "/data/")
         config-path (.getRealPath ^ServletContext context "/config/")]
     (log/debug "calling init: with paramters: "
                root-path ", " data-path ", " config-path)
       (.setAttribute ^ServletContext context "ctbappcontext"
                      (init root-path data-path config-path))))

(defn init-standalone
  "Initialize any needed resources, resource are returned as map to caller."
  []
  (init "." "./data" "./config"))
