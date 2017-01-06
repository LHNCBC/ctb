(ns ctb.umls-indexed
  (:require [clojure.java.io :as io]
            [clojure.string :refer [join split lower-case]]
            [clojure.set :refer [union]]
            [clojure.tools.logging :as log]
            [skr.rrf-mrconso-utils :as rrf-mrconso]
            [skr.rrf-mrsty-utils :as rrf-mrsty]
            [skr.mwi-utilities :as mwi]
            [umls-tables.core :refer [mrconso-line-record-to-map
                                      mrsat-line-record-to-map
                                      mrsty-line-record-to-map]])
  (:import (java.lang System)
           (irutils InvertedFileContainer InvertedFile)))

;; # Inverted Files for UMLS tables
;;
;; UMLS Tables
;;
;; * `*mrconsocui-index*` mrconso
;; * `*mrconsostr-index*` mrconsostr
;; * `*zzsty-index*` mrsty (abbreviated)
;; * `*mrsty-index*` mrstyrrf (full)
;; * `*mrsat-index*` mrsat (To be indexed)


(defn get-container [tablepath indexpath]
  (new InvertedFileContainer tablepath indexpath))

(defn get-index [^InvertedFileContainer container dbname]
  (let [index (.get container dbname)]
    (.update index)
    (.setup index)
    index))

(defn lookup [^InvertedFile index term]
  (vec (.getValue (.lookup index (lower-case term)))))

;; Paths to tables used by Java-based inverted file library
;; use property ctb.ivf.dataroot to set data root of inverted file.
(defonce ^:dynamic *dataroot* (System/getProperty "ctb.ivf.dataroot" "data/ivf/2016AA"))
(defonce ^:dynamic *tablepath* (format "%s/%s" *dataroot* "tables"))
(defonce ^:dynamic *indexpath* (format "%s/%s" *dataroot* "ifindices"))

;; Container for inverted file references

;; (defonce ^:dynamic *container* (get-container *tablepath* *indexpath*))

;; Inverted file references

;; (defonce ^:dynamic *mrconsocui-index* (get-index *container* "mrconso"))
;; (defonce ^:dynamic *mrconsostr-index* (get-index *container* "mrconsostr"))
;; (defonce ^:dynamic *zzsty-index* (get-index *container* "mrsty"))
;; Currently disabled:
;; (defonce ^:dynamic *mrsat-index* (get-index *container* "mrsat"))
;; (defonce ^:dynamic *mrsty-index* (get-index *container* "mrstyrrf"))

(defn init-index
  ([]
   (init-index (System/getProperty "ctb.ivf.dataroot" "data/ivf/2016AA")
                 "tables" "ifindices"))
  ([dataroot tablepath indexpath]
   (if (.exists (io/file dataroot))
     (do
       (def ^:dynamic *dataroot* dataroot)
       (def ^:dynamic *tablepath* (format "%s/%s" dataroot tablepath))
       (def ^:dynamic *indexpath* (format "%s/%s" dataroot indexpath))
       (if (and (.exists (io/file *tablepath*))
                (.exists (io/file *indexpath*)))
         (do 
           (def ^:dynamic *container* (get-container *tablepath* *indexpath*))
           (def ^:dynamic *mrconsocui-index* (get-index *container* "mrconso"))
           (def ^:dynamic *mrconsostr-index* (get-index *container* "mrconsostr"))
           (def ^:dynamic *zzsty-index* (get-index *container* "mrsty"))
           ;;   (def ^:dynamic *mrsat-index* (get-index *container* "mrsat"))
           (def ^:dynamic *mrsty-index* (get-index *container* "mrstyrrf"))
           "indexes initialized")
         (do
           (if (not (.exists (io/file *tablepath*)))
             (log/error (format "ctb.umls-indexed/init-index: tablepath: %s does not exist." *tablepath*))
             (if (not (.exists (io/file *indexpath*)))
               (log/error (format "ctb.umls-indexed/init-index: indexpath: %s does not exist." *indexpath*)))))))
     (log/error (format "dataroot: %s does not exist." dataroot))) ))

(defonce ^:dynamic *memoized-normalize-ast-string* (memoize mwi/normalize-ast-string))

(defn generate-term-conceptid-map
  "Generate map of term -> conceptids"
  [termlist]
  (let [termset (set (map *memoized-normalize-ast-string* termlist))]
    (reduce (fn [newmap term]
              (assoc newmap term
                     (set (map #(:cui (mrconso-line-record-to-map %))
                               (lookup *mrconsostr-index* term)))))
            {} termset)))
  
(defn generate-term-conceptid-set
  "Generate set of conceptids corresponding to termlist"
  [termlist]
  (let [termset (set (map *memoized-normalize-ast-string* termlist))]
    (reduce (fn [newset term]
              (union newset (set (map #(:cui (mrconso-line-record-to-map %))
                                      (lookup *mrconsostr-index* term)))))
            #{} termset)))

(defn generate-cui-concept-map-from-cuiset
  "Generate cui -> concept-records map."
  [cuiset]
  (reduce (fn [newmap cui]
            (assoc newmap cui (concat (newmap cui) (map mrconso-line-record-to-map
                                                        (lookup *mrconsocui-index* cui)))))
          {} cuiset))

(defn get-mrconso-records-with-preferred-name
  "Return str field of first MRCONSO record with TS field equal to \"P\" and STT field equal to \"PF\". "
  [cui]
  (filter #(and (= (:ts %) "P") (= (:stt %) "PF"))
          (map mrconso-line-record-to-map
               (lookup *mrconsocui-index* cui))))

(defn get-preferred-name
  "Return str field of first MRCONSO record with TS field equal to \"P\" and STT field equal to \"PF\". "
  [cui]
  (:str (first (get-mrconso-records-with-preferred-name cui))))

(defn get-mrsat-records
  "Get MRSAT records for cui (concept unique identifier) (currently
  not implemented)."
  [cui]
  ;; (map mrsat-line-record-to-map (lookup *mrsat-index* cui))
  )

(defn get-mrsty-records
  "Get MRSTY records for cui (concept unique identifier)."
  [cui]
  (map mrsty-line-record-to-map
       (lookup *mrsty-index* cui)))


