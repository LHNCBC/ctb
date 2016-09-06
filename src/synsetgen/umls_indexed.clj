(ns synsetgen.umls-indexed
  (:require [clojure.java.io :as io]
            [clojure.string :refer [join split]]
            [clojure.set :refer [union]]
            [skr.rrf-mrconso-utils :as rrf-mrconso]
            [skr.rrf-mrsty-utils :as rrf-mrsty]
            [skr.mwi-utilities :as mwi]
            [umls-tables.core :refer [mrconso-line-record-to-map
                                      mrsat-line-record-to-map
                                      mrsty-line-record-to-map]])
  (:import (irutils InvertedFileContainer)))

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

(defn get-index [container dbname]
  (let [index (.get container dbname)]
    (.update index)
    (.setup index)
    index))

(defn lookup [index term]
  (vec (.getValue (.lookup index (.toLowerCase term)))))

(defonce ^:dynamic *dataroot* "data/ivf/2016AA")
(defonce ^:dynamic *tablepath* (format "%s/%s" *dataroot* "tables"))
(defonce ^:dynamic *indexpath* (format "%s/%s" *dataroot* "ifindices"))
(defonce ^:dynamic *container* (get-container *tablepath* *indexpath*))
(defonce ^:dynamic *mrconsocui-index* (get-index *container* "mrconso"))
(defonce ^:dynamic *mrconsostr-index* (get-index *container* "mrconsostr"))
(defonce ^:dynamic *zzsty-index* (get-index *container* "mrsty"))
;; (defonce ^:dynamic *mrsat-index* (get-index *container* "mrsat"))
(defonce ^:dynamic *mrsty-index* (get-index *container* "mrstyrrf"))



(defn reinit-index
  ([]
   (reinit-index "data/ivf/2016AA" "tables" "ifindices"))
  ([dataroot tablepath indexpath]
   (def ^:dynamic *dataroot* dataroot)
   (def ^:dynamic *tablepath* (format "%s/%s" dataroot tablepath))
   (def ^:dynamic *indexpath* (format "%s/%s" dataroot indexpath))
   (def ^:dynamic *container* (get-container *tablepath* *indexpath*))
   (def ^:dynamic *mrconsocui-index* (get-index *container* "mrconso"))
   (def ^:dynamic *mrconsostr-index* (get-index *container* "mrconsostr"))
   (def ^:dynamic *zzsty-index* (get-index *container* "mrsty"))
;;   (def ^:dynamic *mrsat-index* (get-index *container* "mrsat"))
   (def ^:dynamic *mrsty-index* (get-index *container* "mrstyrrf"))))

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

;; (defn get-mrsat-records
;;   [cui]
;;   (map mrsat-line-record-to-map
;;        (lookup *mrsat-index* cui)))



