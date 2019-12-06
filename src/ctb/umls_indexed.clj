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
;;
;; These are stored in the servlet-context attribute "ctbappcontext".

(defn get-container [tablepath indexpath]
  (new InvertedFileContainer tablepath indexpath))

(defn get-index [^InvertedFileContainer container dbname]
  (let [index (.get container dbname)]
    (.update index)
    (.setup index)
    index))

(defn lookup
  "Lookup term in inverted index. Term is lowercased before search
  unless lower-case? is set to false, then term is unchanged."
  ([^InvertedFile index term]
   (vec (.getValue (.lookup index (lower-case term)))))
  ([^InvertedFile index term lower-case?]
   (if lower-case?
     (lookup index term)
     (vec (.getValue (.lookup index term))))))

;; Container for inverted file references
;;
;;    { :dataroot dataroot
;;      :tablepath tablepath
;;      :indexpath indexpath
;;      :container container
;;
;;    ;; Inverted file references
;;
;;      :mrconsocui-index (get-index container "mrconso")
;;      :mrconsostr-index (get-index container "mrconsostr")
;;      :zzsty-index (get-index container "mrsty")
;;    ;; :mrsat-index (get-index container "mrsat")
;;    ;  :mrsty-index (get-index container "mrstyrrf"))
;;    }

(defn init-index
  "initialize inverted file indexes and return indexes in map."
  ([]
    ;; Paths to tables used by Java-based inverted file library
    ;; use property ctb.ivf.dataroot to set data root of inverted file.
   (init-index (System/getProperty "ctb.ivf.dataroot" "data/ivf/2016AA")
               "tables" "ifindices"))
  ([dataroot tabledir indexdir]
   (let [tablepath (format "%s/%s" dataroot tabledir)
         indexpath (format "%s/%s" dataroot indexdir)]
     (if (and (.exists (io/file dataroot))
              (.exists (io/file tablepath))
              (.exists (io/file indexpath)))
       (let [container (get-container tablepath indexpath)
             mrconsocui-index (get-index container "mrconso")
             mrconsostr-index (get-index container "mrconsostr")
             zzsty-index (get-index container "mrsty")
             mrsty-index (get-index container "mrstyrrf")
             newindex-context (hash-map :dataroot dataroot
                                     :tablepath tablepath
                                     :indexpath indexpath
                                     :container container
                                     :mrconsocui-index mrconsocui-index
                                     :mrconsostr-index mrconsostr-index 
                                     :zzsty-index zzsty-index 
                                     ;; :mrsat-index (get-index container "mrsat")
                                     :mrsty-index mrsty-index )]
         (log/info "indexes initialized")
         newindex-context)
       (do
         (if (not (.exists (io/file dataroot)))
           (log/error (format "dataroot: %s does not exist." dataroot)))
         (if (not (.exists (io/file tablepath)))
           (log/error (format "ctb.umls-indexed/init-index: tablepath: %s does not exist." tablepath)))
         (if (not (.exists (io/file indexpath)))
           (log/error (format "ctb.umls-indexed/init-index: indexpath: %s does not exist." indexpath)))
         {})))))

(defn memoized-normalize-ast-string
  "return memoized version of normalize-ast-string"
  []
  (memoize mwi/normalize-ast-string))

(defn generate-term-conceptid-map
  "Generate map of term -> conceptids"
  [indexes termlist]
  (let [termset (set (map mwi/normalize-ast-string termlist))]
    (reduce (fn [newmap term]
              (assoc newmap term
                     (set (map #(:cui (mrconso-line-record-to-map %))
                               (lookup (:mrconsostr-index indexes) term)))))
            {} termset)))

(defn generate-term-conceptid-set
  "Generate set of conceptids corresponding to termlist"
  [indexes termlist]
  (let [termset (set (map mwi/normalize-ast-string termlist))]
    (reduce (fn [newset term]
              (union newset (set (map #(:cui (mrconso-line-record-to-map %))
                                      (lookup (:mrconsostr-index indexes) term)))))
            #{} termset)))

(defn generate-cui-concept-map-from-cuiset
  "Generate cui -> concept-records map."
  [indexes cuiset]
  (reduce (fn [newmap cui]
            (assoc newmap cui (concat (newmap cui)
                                      (map mrconso-line-record-to-map
                                           (lookup
                                            (:mrconsocui-index indexes) cui false)))))
          {} cuiset))

(defn get-mrconso-records-with-preferred-name
  "Return str field of first MRCONSO record with TS field equal to \"P\" and STT field equal to \"PF\". "
  [indexes cui]
  (filter #(and (= (:ts %) "P") (= (:stt %) "PF"))
          (map mrconso-line-record-to-map
               (lookup (:mrconsocui-index indexes) cui false))))

(defn get-preferred-name
  "Return str field of first MRCONSO record with TS field equal to \"P\" and STT field equal to \"PF\". "
  [indexes cui]
  (:str (first (get-mrconso-records-with-preferred-name indexes cui))))

(defn get-mrsat-records
  "Get MRSAT records for cui (concept unique identifier) (currently
  not implemented)."
  [indexes cui]
  ;; (map mrsat-line-record-to-map (lookup (:mrsat-index indexes) cui))
  )

(defn get-mrsty-records
  "Get MRSTY records for cui (concept unique identifier)."
  [indexes cui]
  (map mrsty-line-record-to-map
       (lookup (:mrsty-index indexes) cui)))


